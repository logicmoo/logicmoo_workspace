%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2010, 2015 Christoph Wernhard
%%%%
%%%% This program is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% This program is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(toyproject, [elim/2,
		       elim_d/2,
		       % elim_c/2,
		       n_size/2,
		       expand_elim_macros/2,
		       define_elim_macro/2,
		       elim_macro/2,
		       xbase/2,
		       xabase/2,
		       %%
		       %% Recording elimination tasks.
		       %%
		       elimination_task_to_cnf/4,
		       elimination_task_to_defcnf/4,
		       recorded_elimination_task/1,
		       rec/1,
		       %% n_varelim_args/5,
		       %% n_varelim_args_def/5,
		       %%
		       %% Support for predicate groups.
		       %%
		       get_group/2,
		       set_group/3,
		       add_group/3,
		       add_second_group/3,
		       map_set_group/3,
		       %%
		       %% Experimental stuff
		       rr0/2,
		       rr0/3,
		       rr1/3,
		       rr_fact/3,
		       rr_comp/3
		       ]).


:- set_prolog_flag(optimise,true).
:- use_module(swilib(pretty)).
:- use_module(swilib(err)).
:- use_module(swilib(info)).
:- use_module(swilib(hash)).
:- use_module(swilib(sysdep)).
:- use_module(nf(nf)).
:- use_module(nf(nfutils)).
:- use_module('lrpo').
:- use_module('auxiliary').
:- use_module('config').
:- use_module('external').

:- discontiguous r/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic rec/1.

:- flag(record_mode, _, false).

set_record_mode(false) :-
	retractall(rec(_)),
	flag(record_mode, _, false).

set_record_mode(true) :-
	retractall(rec(_)),
	flag(record_mode, _, true).

record_mode :-
	flag(record_mode, true, true).


%%%% 
%%%% Elimination of proj and forg always yields formulas that satisfy also
%%%% the syntactic property that they do not contain literals that are not
%%%% in the scope of projection or in the scope of forgetting,
%%%% respectively.
%%%% 

elim(F, G) :-
	( get_conf(version, 1) ->
	  err('Old implementation is no longer supported')
	; elim_new(F, G)
	).

fix_random :-
	set_random(seed(10)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% BASED ON THE OLD IMPLEMENTATION
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_negate(+A, -A).
n_negate(-A, +A).
n_negate(true, false).
n_negate(false, true).
n_negate((F,G), (F1;G1)) :-
	n_negate(F, F1),
	n_negate(G, G1).
n_negate((F;G), (F1,G1)) :-
	n_negate(F, F1),
	n_negate(G, G1).


f_n(F, N) :-
	f_to_n_1(F, N1),
	n_simplify_inner(N1, N).

n_f(N, F) :-
	replace_literal(N, -X, ~X, N1),
	replace_literal(N1, +X, X, F).

n_unlink_1(F, As, F1) :-
	rewrite_step_inner_first(unlink(As), F, F2),
	!,
	n_simplify_inner(F2, F3),
	n_unlink_1(F3, As, F1).
n_unlink_1(F, _, F).


n_elim_lits(Lits, F, F1) :-
	( get_conf(record_mode, true) ->
	  asserta(rec(n_elim_lits(Lits, F, F1))),
	  statistics(cputime, TBefore)
	; true
	),
	n_elim_lits_1(Lits, F, F1),
	( get_conf(record_mode, true) ->
	  statistics(cputime, TAfter),
	  T is TAfter - TBefore,
	  asserta(rec(time(T, n_elim_lits(Lits, F, _))))
	; true
	).

:- dynamic last_elim/2.


n_elim_via_rew(Lits, F, F1) :-
	retractall(last_elim(_, _)),
%	asserta(last_elim(Lits, F)),
	length(Lits, LEN),
	info(60, 'elim: vars:~| ~t~w~7+', [LEN]),
	Lits = [_|_],
	lits_atoms(Lits, As),
%	writeq(all_links_in(As, F, As1)), nl,
	all_links_in(As, F, As1),
 	n_unlink_if_cheap(F, As1, F2),
	all_links_in(As, F2, As2),
	n_el_1(Lits, As2, F2, F3, Lits1),
	( Lits1 = Lits ->
	  F4 = F3
	; n_simplify_inner(F3, F4)
	),  
	( Lits1 = Lits, %% expensive only if no progress otherwise
	  Lits1 = [L|Lits2] ->
	  lit_atom(L, A),
	  n_size(F, Size),
	  info(20, 'Expensively unlinking ~w, source size: ~w', [A, Size]),
	  n_unlink_1(F4, [A], F5),
	  replace_literal(F5, L, true, F6),
	  n_simplify_inner(F6, F7)
	; Lits2 = Lits1,
          F7 = F4
	),
	n_elim_via_rew(Lits2, F7, F1).
n_elim_via_rew([], F, F).
	
n_el_1([L|Ls], As, F, F1, [L|Ls1]) :-
	lit_atom(L, A),
	memberchk(A, As),
	!,
	n_el_1(Ls, As, F, F1, Ls1).
n_el_1([L|Ls], As, F, F1, Ls1) :-
	replace_literal(F, L, true, F2),
	n_el_1(Ls, As, F2, F1, Ls1).
n_el_1([], _, F, F, []).



n_is_unlinked(F, A) :-
	\+ n_contains_link(F, A).

% n_is_unlinked(F, A) :- 
% 	rewrite_step_inner_first(unlink([A]), F, _),
% 	!,
% 	fail.
% n_is_unlinked(_, _).


%%%% 
%%%% DEBUG
%%%% 
n_unlink_if_cheap(F, [], F) :-
	!.
n_unlink_if_cheap(F, As, F1) :-
	rewrite_step_inner_first(unlink(As), F, G2),
	n_size(F, N0),
	n_simplify_rough(G2, G3),
	n_size(G3, NG),
 	( NG =< N0 ->
 	  F5 = G3,
 	  N5 = NG
	; rewrite_step_outer_first(unlink(As), F, H2),
 	  n_simplify_rough(H2, H3),
 	  n_size(H3, NH),
 	  ( NH =< N0 ->
 	    F5 = H3,
 	    N5 = NH
 	  ; ( NH < NG ->
	      F4 = H3,
	      N4 = NH
	    ; F4 = G3,
	      N4 = NG
	    ),
	    ( N4 =< N0 + 8 ->
	      F5 = F4,
	      N5 = N4
	    ; n_simplify_inner(F4, F5),
 	      n_size(F5, N5),
	      N5 =< N0 + 8
	    )
	  )
	),
	info(60, 'Unlink cheap from: ~w to: ~w', [N0, N5]),
	!,
	all_links_in(As, F5, As1),
	n_unlink_if_cheap(F5, As1, F1).
n_unlink_if_cheap(F, _, F).



% 
% n_unlink_all_if_cheap(F, F1) :-
% 	n_size(F, N0),
% %	info(3, 'Size: ~w', [N0]),
% 	rewrite_step_inner_first(unlink_all, F, F2),
% 	n_simplify_inner(F2, F3),
% 	n_size(F3, N1),
% %	N1 =< N0 + 4,
% 	N1 < N0,
% %	writeln(unlink_all(N1-N0)),
% 	!,
% 	n_unlink_all_if_cheap(F3, F1).
% n_unlink_all_if_cheap(F, F).
% 

n_elim_atom(F, A, F1) :-
	n_unlink_1(F, [A], F2),
	replace_literal(F2, +A, true, F3),
	replace_literal(F3, -A, true, F4),
	n_simplify_inner(F4, F1).

n_elim_lit(F, L, F1) :-
	lit_atom(L, A),
	n_unlink_1(F, [A], F2),
	replace_literal(F2, L, true, F3),
	n_simplify_inner(F3, F1).



r(simp, (F,(F,H)), (F,H)).
r(simp, (F;(F;H)), (F;H)).
r(simp, (F,F), F).
r(simp, (F;F), F).

%%
%% not needed if truth values are already eliminated by fast_tv
%%
r(simp, (true,F), F).
r(simp, (false, _), false).
r(simp, (true;_), true).
r(simp, (false; F), F).
r(simp, (F,true), F).
r(simp, (_,false), false).
r(simp, (_;true), true).
r(simp, (F;false), F).


r(simp_cheap, (F,(F,H)), (F,H)).
r(simp_cheap, (F;(F;H)), (F;H)).
r(simp_cheap, (F,F), F).
r(simp_cheap, (F;F), F).
% r(simp_cheap, (true,F), F).
% r(simp_cheap, (false, _), false).
% r(simp_cheap, (true;_), true).
% r(simp_cheap, (false; F), F).
% r(simp_cheap, (F,true), F).
% r(simp_cheap, (_,false), false).
% r(simp_cheap, (_;true), true).
% r(simp_cheap, (F;false), F).
% r(simp_cheap, (+A, F), (+A, F1)) :-
% 	replace_atom_with_truthvalue_and_test(F, A, true, F1).
% r(simp_cheap, (-A, F), (-A, F1)) :-
% 	replace_atom_with_truthvalue_and_test(F, A, false, F1).
% r(simp_cheap, (+A; F), (+A; F1)) :-
% 	replace_atom_with_truthvalue_and_test(F, A, false, F1).
% r(simp_cheap, (-A; F), (-A; F1)) :-
% 	replace_atom_with_truthvalue_and_test(F, A, true, F1).


%%%% 
%%%% Pullout. Tested Version.
%%%% 
r(simp, (+A, F), (+A, F1)) :-
	replace_atom_with_truthvalue_and_test(F, A, true, F1).
r(simp, (-A, F), (-A, F1)) :-
	replace_atom_with_truthvalue_and_test(F, A, false, F1).
r(simp, (+A; F), (+A; F1)) :-
	replace_atom_with_truthvalue_and_test(F, A, false, F1).
r(simp, (-A; F), (-A; F1)) :-
	replace_atom_with_truthvalue_and_test(F, A, true, F1).

% %%%% 
% %%%% Pullout. Experimental.
% %%%% 
% r(simp, (+A, F), F1) :-
% 	replace_atom_with_truthvalue_and_test(F, A, true, F2),
% 	n_simp_tv_fast((+A, F2), F1).
% r(simp, (-A, F), F1) :-
% 	replace_atom_with_truthvalue_and_test(F, A, false, F2),
% 	n_simp_tv_fast((-A, F2), F1).
% r(simp, (+A; F), F1) :-
% 	replace_atom_with_truthvalue_and_test(F, A, false, F2),
% 	n_simp_tv_fast((+A; F2), F1).
% r(simp, (-A; F), F1) :-
% 	replace_atom_with_truthvalue_and_test(F, A, true, F2),
% 	n_simp_tv_fast((-A; F2), F1).


%%%% r(simp, (F,G), (G,F)) :-
%%%% 	G @< F. 
%%%% r(simp, (F,(G,H)), (G,(F,H))) :-
%%%% 	G @< F.
%%%% r(simp, (F;G), (G;F)) :-
%%%% 	G @< F. 	
%%%% r(simp, (F;(G;H)), (G;(F;H))) :-
%%%% 	G @< F.
%%%% 
%%%% r(simp, ((F,G),H), (F,(G,H))) :-
%%%% 	F @< (G,H).
%%%% r(simp, ((F;G);H), (F;(G;H))) :-
%%%% 	F @< (G;H).
%%%% 

%%%% r(simp, (F,G), (G,F)) :-
%%%% 	basic(G), complex(F).
%%%% r(simp, (F;G), (G;F)) :-
%%%% 	basic(G), complex(F).
%%%% r(simp, (F,(G,H)), (G,(F,H))) :-
%%%% 	basic(G),
%%%% 	complex(F).
%%%% r(simp, (F;(G;H)), (G;(F;H))) :-
%%%% 	basic(G),
%%%% 	complex(F).
%%%% r(simp, ((F,G),H), (F,(G,H))).
%%%% r(simp, ((F;G);H), (F;(G;H))). %% ?


r(simp, (F,G), (G,F)) :-
	lrpo_greater(F, G).
r(simp, (F;G), (G;F)) :-
	lrpo_greater(F, G).
r(simp, (F,(G,H)), (G,(F,H))) :-
	lrpo_greater(F, G).
r(simp, (F;(G;H)), (G;(F;H))) :-
	lrpo_greater(F, G).


% r(simp, (F,G), FG) :-
% 	conjunction_to_list((F,G), FG1),
% 	sort(FG1, FG2),
% 	length(FG1, L1),
% 	length(FG2, L2),
% 	L2 < L1,
% 	list_to_conjunction(FG2, FG).
% r(simp, (F;G), FG) :-
% 	disjunction_to_list((F;G), FG1),
% 	sort(FG1, FG2),
% 	length(FG1, L1),
% 	length(FG2, L2),
% 	L2 < L1,
% 	list_to_disjunction(FG2, FG).

basic(true).
basic(false).
basic(+_).
basic(-_).
complex((_,_)).
complex((_;_)).

% r_3(unlink(As), (F,G), ((+A,FG1);(-A,FG2))) :-
% 	member(A, As),
% 	( n_lit(F, +A), n_lit(G, -A)
% 	; n_lit(F, -A), n_lit(G, +A)
% 	),
% 	!,
% 	replace_atom_with_truthvalue((F,G), A, true, FG1),
% 	replace_atom_with_truthvalue((F,G), A, false, FG2).

r_3(unlink(As), (F,G), (FShared, ((+A,FA1);(-A,FA2)))) :-
	member(A, As),
	( n_lit(F, +A), n_lit(G, -A)
	; n_lit(F, -A), n_lit(G, +A)
	),
	!,
	unlink_split_shared_conjuncts(A, (F,G), FA, FShared),
	replace_atom_with_truthvalue(FA, A, true, FA1),
	replace_atom_with_truthvalue(FA, A, false, FA2).


% r_3(unlink_all, (F,G), (FShared, ((+A,FA1);(-A,FA2)))) :-
% 	n_lit(F, L),
% 	lit_complement(L, L1),
% 	n_lit(G, L1),
% 	!,
% 	lit_atom(L, A),
% 	unlink_split_shared_conjuncts(A, (F,G), FA, FShared),
% 	replace_atom_with_truthvalue(FA, A, true, FA1),
% 	replace_atom_with_truthvalue(FA, A, false, FA2).



unlink_split_shared_conjuncts(A, F, F1, G1) :-
	conjunction_to_list(F, Fs),
	split_contains_atom(Fs, A, F1s, G1s),
	list_to_conjunction(F1s, F1),
	list_to_conjunction(G1s, G1).

split_contains_atom([F|Fs], A, [F|Fs1], Gs1) :-
	n_contains_atom(F, A),
	!,
	split_contains_atom(Fs, A, Fs1, Gs1).
split_contains_atom([F|Fs], A, Fs1, [F|Gs1]) :-
	split_contains_atom(Fs, A, Fs1, Gs1).
split_contains_atom([], _, [], []).


tv_complement(true, false).
tv_complement(false, true).

%%%% 
%%%% Suceeds just if a replacement has been performed
%%%% 
replace_atom_with_truthvalue_and_test(F, A, TV, F1) :-
	n_contains_atom(F, A), %% pre-test to improve performance
	replace_atom_with_truthvalue(F, A, TV, F1),
	F1 \= F.
	
n_contains_atom(+A, A) :-
	!.
n_contains_atom(-A, A) :-
	!.
n_contains_atom((F,G), A) :-
	!,
	( n_contains_atom(F, A)
	; n_contains_atom(G, A)
	).
n_contains_atom((F;G), A) :-
	!,
	( n_contains_atom(F, A)
	; n_contains_atom(G, A)
	).

n_contains_literal(L, L) :-
	!.
n_contains_literal((F,G), L) :-
	!,
	( n_contains_literal(F, L)
	; n_contains_literal(G, L)
	).
n_contains_literal((F;G), L) :-
	!,
	( n_contains_literal(F, L)
	; n_contains_literal(G, L)
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all_links_in(Atoms, F, Atoms1) :-
 	all_links_in_v1(Atoms, F, Atoms1).
	
all_links_in_v1(As, F, As1) :-
  	findall(A, (member(A, As), n_contains_link(F, A)), As1).

map_lit_complement([+X|Xs], [-X|Xs1]) :-
	!,
	map_lit_complement(Xs, Xs1).
map_lit_complement([-X|Xs], [+X|Xs1]) :-
	!,
	map_lit_complement(Xs, Xs1).
map_lit_complement([], []).

map_lit_atom([+X|Xs], [X|Xs1]) :-
	!,
	map_lit_atom(Xs, Xs1).
map_lit_atom([-X|Xs], [X|Xs1]) :-
	!,
	map_lit_atom(Xs, Xs1).
map_lit_atom([], []).


n_contains_link((F,G), A) :-
	!,
	( n_contains_literal(F, +A), n_contains_literal(G, -A) ->
	  true
	; n_contains_literal(F, -A), n_contains_literal(G, +A) ->
	  true
	; n_contains_link(F, A) ->
	  true
	; n_contains_link(G, A)
	).
n_contains_link((F;G), A) :-
	!,
	( n_contains_link(F, A) ->
	  true
	; n_contains_link(G, A)
	).

replace_atom_with_truthvalue(F, A, TV, F1) :-
 	tv_complement(TV, TV1),
 	replace_literal(F, +A, TV, F2),
 	replace_literal(F2, -A, TV1, F1).

replace_atom(F, A, A1, F) :-
	A == A1,
	!.
replace_atom(F, A, A1, F1) :-
	replace_literal(F, +A, +A1, F2),
	replace_literal(F2, -A, -A1, F1).

replace_literal(F, Old, New, F) :-
	Old == New,
	!.
replace_literal((F,G), Old, New, (F1,G1)) :-
	!,
	replace_literal(F, Old, New, F1),
	replace_literal(G, Old, New, G1).
replace_literal((F;G), Old, New, (F1;G1)) :-
	!,
	replace_literal(F, Old, New, F1),
	replace_literal(G, Old, New, G1).
replace_literal(~(F), Old, New, ~(F1)) :-
	( F = (_,_) ; F = (_;_) ),
	!,
	replace_literal(F, Old, New, F1).
replace_literal(F, Old, New, G) :-
	copy_term(Old-New, F-G),
	!.
replace_literal(F, _, _, F).

%% old implementation:
%%
% replace_literal(F, Old, New, F) :-
%  	Old == New,
%  	!.
% replace_literal(F, Old, New, F1) :-
% 	subexpression(F, Old1, F2, New1),
% 	copy_term(Old-New, Old1-New1),
% 	!,
% 	replace_literal(F2, Old, New, F1).
% replace_literal(F, _, _, F).

replace_atoms(F, [A-A1|AA1s], F1) :-
	replace_atom(F, A, A1, F2),
	replace_atoms(F2, AA1s, F1).
replace_atoms(F, [], F).

rewrite_simp_cheap(F, F1) :-
	rewrite_v2(simp_cheap, F, F1).

rewrite_simp(F, F1) :-
	rewrite_v2(simp, F, F1).

rewrite_v1(P, F, F1) :-
	subformula_n(F, S, F2, T),
	r(P, S, T),
	!,
	rewrite_v1(P, F2, F1).
rewrite_v1(_, F, F).


%%%% 
%%%% experimental, might be faster than rewrite_v1
%%%% 
rewrite_v2(P, F, F1) :-
	subformula_n(F, S, F2, T),
	r(P, S, T1),
	!,
	rewrite_v2(P, T1, T),
	rewrite_v2(P, F2, F1).
rewrite_v2(_, F, F).

%%%% 
%%%% experimental
%%%% 
rewrite_v3(P, F, F1) :-
 	subformula_inner_first_n(F, S, F2, T),
 	r(P, S, T),
 	!,
 	rewrite_v3(P, F2, F1).
rewrite_v3(_, F, F).



% %%% experimental:
% rewrite_inner_first(F, P, F1) :-
% 	subexpression_inner_first(F, S, F2, T),
% 	r_3(P, S, T),
% 	!,
% 	rewrite_inner_first(F2, P, F1).
% rewrite_inner_first(F, _, F).


rewrite_step_inner_first(P, F, F1) :-
	subformula_inner_first_n(F, S, F1, T),
	r_3(P, S, T),
	!.

rewrite_step_outer_first(P, F, F1) :-
	subformula_n(F, S, F1, T),
	r_3(P, S, T),
	!.

subformula_n(F, F, G, G).
subformula_n((F1, F2), S, (G1, F2), T) :-
	subformula_n(F1, S, G1, T).
subformula_n((F1, F2), S, (F1, G2), T) :-
	subformula_n(F2, S, G2, T).
subformula_n((F1; F2), S, (G1; F2), T) :-
	subformula_n(F1, S, G1, T).
subformula_n((F1; F2), S, (F1; G2), T) :-
	subformula_n(F2, S, G2, T).

subformula_inner_first_n((F1, F2), S, (G1, F2), T) :-
	subformula_inner_first_n(F1, S, G1, T).
subformula_inner_first_n((F1, F2), S, (F1, G2), T) :-
	subformula_inner_first_n(F2, S, G2, T).
subformula_inner_first_n((F1; F2), S, (G1; F2), T) :-
	subformula_inner_first_n(F1, S, G1, T).
subformula_inner_first_n((F1; F2), S, (F1; G2), T) :-
	subformula_inner_first_n(F2, S, G2, T).
subformula_inner_first_n(F, F, G, G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f_to_n_1(F, N) :-
	rewrite_v1(fn_1, F, F1),
	f_to_n_2(F1, N).


r(fn_1, (F -> G), (~F;G)).
r(fn_1, (F <- G), (F;~G)).
r(fn_1, (F <-> G), ((F,G);(~F,~G))).
r(fn_1, ~((F,G)), (~F;~G)).
r(fn_1, ~((F;G)), ((~F,~G))).
r(fn_1, ~(~(F)), F).

f_to_n_2((F,G), (F1,G1)) :-
	!,
	f_to_n_2(F, F1),
	f_to_n_2(G, G1).
f_to_n_2((F;G), (F1;G1)) :-
	!,
	f_to_n_2(F, F1),
	f_to_n_2(G, G1).
% f_to_n_2(xproj(S,F), xproj(S,F1)) :-
% 	!,
% 	
% f_to_n_2(xraise(S,F), -F) :-
% 	!.
% f_to_n_2(xcirc(S,F), -F) :-
% 	!.
% 
f_to_n_2(~F, -F) :-
	!.
f_to_n_2(F, +F).


n_size((F,G), N) :-
	!,
	n_size(F, N1),
	n_size(G, N2),
	N  is N1 + N2.
n_size((F;G), N) :-
	!,
	n_size(F, N1),
	n_size(G, N2),
	N  is N1 + N2.
n_size(_, 1).

s_atomset(S, SIG) :-
	findall(A, (member(L, S), lit_atom(L, A)), SIG1),
	sort(SIG1, SIG).

atomset_subtract(S1, S2, S3) :-
	ord_subtract(S1, S2, S3).

atomset_union(S1, S2, S3) :-
	ord_union(S1, S2, S3).

litsig_subtract(S1, S2, S3) :-
	ord_subtract(S1, S2, S3).

litsig_intersect(S1, S2, S3) :-
	ord_intersect(S1, S2, S3).

n_atomset(F, SIG) :-
	findall(A, n_atom(F, A), SIG1),
	sort(SIG1, SIG).

n_scope(F, SIG) :-
	findall(A, n_lit(F, A), SIG1),
	sort(SIG1, SIG).

lit_atom(+A, A).
lit_atom(-A, A).

lit_complement(+A, -A).
lit_complement(-A, +A).

atom_lit(A, +A).
atom_lit(A, -A).

lits_atoms(Ls, As) :-
	lits_atoms_1(Ls, As1),
	sort(As1, As).

lits_atoms_1([L|As], [A|As1]) :-
	lit_atom(L, A),
	lits_atoms_1(As, As1).
lits_atoms_1([], []).


n_atom(F, A) :-
	n_lit(F, L),
	lit_atom(L, A).

n_lit(F, L) :-
	( F = (F1,F2)
	; F = (F1;F2)
	),
	!,
	( n_lit(F1, L)
	; n_lit(F2, L)
	).
n_lit(+A, +A).
n_lit(-A, -A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SCOPES (derived from stable/scomp.pl)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% A scope specifier is a list of scope specifier elements.
%%%%
%%%% Scope specifier elements are patterns with Prolog variables.
%%%%
%%%% In a specifier element, the special predicate symbol 'X' matches all
%%%% predicates.
%%%% 


% might need some rework (circ requires stuff mentioned in the scope
% and the formula)
%
% patterns here, filled with F's signature.
% for circ, the scope may be larger that F's signature,
%   and patterns have to be filled with all function symbols
% additional to consider in future versions:
% - expansion over all terms for circ/rai
% - global signature for circ/rai

%%%% 
%%%% Members of the scope that do not occur in F are allowed
%%%% to be omitted, which suffices for projection/forgetting.
%%%%
canonic_scope_delimited(complements(S), F, S1) :-
	!,
	scopespec_complements(S, S2),
	canonic_scope_delimited(S2, F, S1).
canonic_scope_delimited(S, F, S1) :-
	n_atomset(F, Sig),
	expand_scope_delimited(S, Sig, S2),
	sort(S2, S1).

expand_scope_delimited(xbase(G), Sig, Xs1) :-
	!,
	map_posneg(Sig, Sig1),
	sort(Sig1, Sig2),
	xbase_internal(G, Sig3),
	ord_intersect(Sig3, Sig2, Xs1).
expand_scope_delimited(xabase(G), Sig, Xs1) :-
	!,
	map_posneg(Sig, Sig1),
	sort(Sig1, Sig2),
	xabase_internal(G, Sig3),
	ord_intersect(Sig3, Sig2, Xs1).
expand_scope_delimited(pos, Sig, Xs1) :-
	!,
	map_pos(Sig, Xs1).
expand_scope_delimited(neg, Sig, Xs1) :-
	!,
	map_neg(Sig, Xs1).
expand_scope_delimited(S, Sig, Xs1) :-
	map_esd(S, Sig, Xs1).
	
map_esd([+A|Xs], Sig, Xs1) :-
	!,
	findall(+B, (member(B, Sig), scope_pattern_subsumes_chk(A, B)), Xs2),
	append(Xs2, Xs3, Xs1),
	map_esd(Xs, Sig, Xs3).
map_esd([-A|Xs], Sig, Xs1) :-
	!,
	findall(-B, (member(B, Sig), scope_pattern_subsumes_chk(A, B)), Xs2),
	append(Xs2, Xs3, Xs1),
	map_esd(Xs, Sig, Xs3).
map_esd([A|Xs], Sig, Xs1) :-
	findall(B,
		( member(C, Sig),
		  scope_pattern_subsumes_chk(A, C),
		  (B = +C ; B = -C)),
		Xs2),
	append(Xs2, Xs3, Xs1),
	map_esd(Xs, Sig, Xs3).
map_esd([], _, []).

scope_pattern_subsumes_chk(Pattern, Term) :-
	number(Pattern),
	!,
	get_group(Term, Pattern).
scope_pattern_subsumes_chk(Pattern, Term) :-
	functor(Pattern, 'X', _),
	!,
	Pattern =.. [_|Pattern1],
	Term =.. [_|Term1],
	subsumes_chk(Pattern1, Term1).
scope_pattern_subsumes_chk(Pattern, Term) :-	
	subsumes_chk(Pattern, Term).

scopespec_complements(pos, neg) :-
	!.
scopespec_complements(neg, pos) :-
	!.
scopespec_complements(complements(S), S) :-
	!.
scopespec_complements(S, S1) :-
	map_sc(S, S1).

map_sc([+X|Xs], [-X|Xs1]) :-
	!,
	map_sc(Xs, Xs1).
map_sc([-X|Xs], [+X|Xs1]) :-
	!,
	map_sc(Xs, Xs1).
map_sc([X|Xs], [X|Xs1]) :-
	!,
	map_sc(Xs, Xs1).
map_sc([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% For raising and circumscription. Based on the corresponding
%%%% first-order signature (with just constant functions) of the
%%%% formula and the scope specifier.
%%%%
%%%% Known limitation: Scope specifiers "pos" and "neg" use scopes just
%%%% determined by the signature of the formula, but a larger scope might be
%%%% intended for circumscription and raising.  A similar effect might result
%%%% if patterns are used as scope specifiers.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

canonic_scope_full(complements(S), F, S1) :-
	!,
	scopespec_complements(S, S2),
	canonic_scope_full(S2, F, S1).
canonic_scope_full(S, F, S1) :-
	n_atomset(F, Sig),
	esf_s_matrix(S, Matrix),
	m_signature([Sig|Matrix], Ps, Fs),
	folsig_del_specials(Ps, Ps1),
	expand_scope_full(S, Ps1, Fs, S2),
	sort(S2, S1).

folsig_del_specials(['X'/_|Ps], Ps1) :-
	!,
	folsig_del_specials(Ps, Ps1).
folsig_del_specials([N/_|Ps], Ps1) :-
	number(N),
	!,
	folsig_del_specials(Ps, Ps1).
folsig_del_specials([P|Ps], [P|Ps1]) :-
	folsig_del_specials(Ps, Ps1).
folsig_del_specials([], []).

esf_s_matrix(pos, []) :-
	!.
esf_s_matrix(neg, []) :-
	!.
esf_s_matrix([+A|Xs], [[A]|Xs1]) :-
	!,
	esf_s_matrix(Xs, Xs1).
esf_s_matrix([-A|Xs], [[A]|Xs1]) :-
	!,
	esf_s_matrix(Xs, Xs1).
esf_s_matrix([A|Xs], [[A]|Xs1]) :-
	!,
	esf_s_matrix(Xs, Xs1).
esf_s_matrix([], []).


expand_scope_full(xbase(G), _, _, Xs1) :-
	!,
	xbase_internal(G, Xs1).
expand_scope_full(xabase(G), _, _, Xs1) :-
	!,
	xabase_internal(G, Xs1).
expand_scope_full(pos, Ps, Fs, Xs1) :-
	!,
%	info(1, 'Warning: Implicit scope specifier pos used for raising'),
	expand_folsig(_, Ps, Fs, Sig),
	map_pos(Sig, Xs1).
expand_scope_full(neg, Ps, Fs, Xs1) :-
	!,
%	info(1, 'Warning: Implicit scope specifier neg used for raising'),
	expand_folsig(_, Ps, Fs, Sig),
	map_neg(Sig, Xs1).
expand_scope_full(S, Ps, Fs, Xs1) :-
	map_esf(S, Ps, Fs, Xs1).

map_esf([+A|Xs], Ps, Fs, Xs1) :-
	!,
	expand_folsig(A, Ps, Fs, Sig),
	map_pos(Sig, Xs2),
%	writeq(x(A,Ps,Fs,Sig,Xs2)), nl,
	append(Xs2, Xs3, Xs1),
	map_esf(Xs, Ps, Fs, Xs3).
map_esf([-A|Xs], Ps, Fs, Xs1) :-
	!,
	expand_folsig(A, Ps, Fs, Sig),
	map_neg(Sig, Xs2),
	append(Xs2, Xs3, Xs1),
	map_esf(Xs, Ps, Fs, Xs3).
map_esf([A|Xs], Ps, Fs, Xs1) :-
	!,
	expand_folsig(A, Ps, Fs, Sig),
	map_posneg(Sig, Xs2),
	append(Xs2, Xs3, Xs1),
	map_esf(Xs, Ps, Fs, Xs3).
map_esf([], _, _, []).


expand_folsig(N, Ps, Fs, Sig) :-
	number(N),
	!,
	expand_folsig(_, Ps, Fs, Sig1),
	map_set_group(Sig1, N, Sig).
expand_folsig(Pattern, Ps, Fs, Sig) :-
	findall(Mem, folsig_member(Pattern, Ps, Fs, Mem), Sig).

map_set_group([X|Xs], Y1, [X1|Xs1]) :-
	set_group(X, Y1, X1),
	map_set_group(Xs, Y1, Xs1).
map_set_group([], _, []).


get_group(A, Group) :-
	functor(A, F, _),
	atom_chars(F, F1),
	reverse(F1, [K|_]),
	( char_type(K, digit) ->
	  number_chars(Group, [K])
	; Group = 0
	).

%%%% 
%%%% Accumulate up to three binary group settings
%%%% 
add_group(A, Group, GroupedAtom) :-
	get_group(A, Group1),
	Group2 is 2 * Group1 + Group,
	set_group(A, Group2, GroupedAtom).

%%%% 
%%%% Accumulate up to three binary group settings
%%%% 
add_second_group(A, Group, GroupedAtom) :-
	get_group(A, Group1),
	Group2 is 2 * Group + Group1,
	set_group(A, Group2, GroupedAtom).


set_group(A, Group, GroupedAtom) :-
	A =.. [F|Args],
	atom_chars(F, F1),
	reverse(F1, [K|F2]),
	( char_type(K, digit) ->
	  F3 = F2
	; F3 = [K|F2]
	),
	( Group = 0 ->
	  reverse(F3, F4)
	; number_chars(Group, [G1]),
	  reverse([G1|F3], F4)
	),
	atom_chars(F5, F4),
	GroupedAtom =.. [F5|Args].


folsig_member(Pattern, Ps, _, Mem) :-
	ground(Pattern),
	!,
	( functor(Pattern, 'X', N) ->
	  Pattern =..  [_|Args],
	  member(P/N, Ps),
	  Mem =.. [P|Args]
	; Mem = Pattern
	).
folsig_member(Pattern, Ps, Fs, Mem) :-
	nonvar(Pattern),
	!,
	Pattern =.. [Pred|Args],
	( Pred = 'X' ->
          functor(Pattern, _, N),
	  member(P/N, Ps),
	  Mem =.. [P|Args],
          folsig_argmembers(Args, Fs)
	; Mem = Pattern,
	  folsig_argmembers(Args, Fs)
	).
folsig_member(Pattern, Ps, Fs, Pattern) :-
	member(P/N, Ps),
	functor(Pattern, P, N),
	Pattern =.. [P|Args],
	folsig_argmembers(Args, Fs).

folsig_argmembers([X|Xs], Fs) :-
	ground(X),
	!,
	folsig_argmembers(Xs, Fs).
folsig_argmembers([X|Xs], Fs) :-
	%% instantiate just with constants, to ensure finite expansion
	member(X/0, Fs),
	folsig_argmembers(Xs, Fs).
folsig_argmembers([], _).


% expand_scope([+A|Xs], Sig, Xs1) :-
% 	number(A),
% 	!,
% 	num_sig(Sig, A, Sig1),
% 	map_pos(Sig1, Sig2),
% 	append(Sig2, Xs2, Xs1),
% 	expand_scope(Xs, Sig, Xs2).
% expand_scope([-A|Xs], Sig, Xs1) :-
% 	number(A),
% 	!,
% 	num_sig(Sig, A, Sig1),
% 	map_neg(Sig1, Sig2),
% 	append(Sig2, Xs2, Xs1),
% 	expand_scope(Xs, Sig, Xs2).
% expand_scope([A|Xs], Sig, Xs1) :-
% 	number(A),
% 	!,
% 	num_sig(Sig, A, Sig1),
% 	map_posneg(Sig1, Sig2),
% 	append(Sig2, Xs2, Xs1),
% 	expand_scope(Xs, Sig, Xs2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Numbers to identify scopes of predicate groups
%%%% 

map_pos([X|Xs], [+X|Xs1]) :-
	map_pos(Xs, Xs1).
map_pos([], []).

map_neg([X|Xs], [-X|Xs1]) :-
	map_neg(Xs, Xs1).
map_neg([], []).

map_posneg([X|Xs], [+X, -X|Xs1]) :-
	map_posneg(Xs, Xs1).
map_posneg([], []).

num_sig(Sig, N, Scope) :-
	map_rm_num(Sig, Sig1),
	map_add_num(Sig1, N, Scope).

add_num(A, N, A1) :-
	A =.. [F|Args],
	concat_atom([F, N], F1),
	A1 =.. [F1|Args].

rm_num(A, A1) :-
	A =.. [F|Args],
	atom_chars(F, F1),
	reverse(F1, [K|F2]),
	( char_type(K, digit) ->
	  F3 = F2
	; F3 = [K|F2]
	),
	reverse(F3, F4),
	atom_chars(F5, F4),
	A1 =.. [F5|Args].

map_add_num([X|Xs], N, [X1|Xs1]) :-
	add_num(X, N, X1),
	map_add_num(Xs, N, Xs1).
map_add_num([], _, []).

map_rm_num([X|Xs], [X1|Xs1]) :-
	rm_num(X, X1),
	map_rm_num(Xs, Xs1).
map_rm_num([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Circumscription, Raising
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

raise_n_form(S, N, ForgLits, Form) :-
	scope_atomsets_pos_neg_bi(S, SPOS, SNEG, SBI),
	raise_pos_1(SPOS, NL1, NL2, T1),
	raise_neg_1(SNEG, NL3, NL4, T2),
	append(NL1, NL3, NL5),
	list_to_conjunction(NL5, N5),
	append(NL2, NL4, NL6),
	list_to_disjunction(NL6, N6),
	append(T1, T2, T3),
	n_atomset(N, SIGN),
	atomset_subtract(SIGN, SBI, SIGVAR),
	map_genpred(SIGVAR, T4),
	append(T3, T4, TABLE),
	replace_atoms(N, TABLE, N2),
	( setof(L, A^B^(member(A-B, TABLE), atom_lit(B, L)), ForgLits ) ->
	  true
	; ForgLits = []
	),
	Form = (N2, N5, N6).

map_genpred([A|As], [A-P|APs]) :-
	genpred(P),
	map_genpred(As, APs).
map_genpred([], []).

raise_pos_1([A|As], [(-A1;+A)|NL1], [(-A1,+A)|NL2], [A-A1|TABLE]) :-
	genpred(A1),
	raise_pos_1(As, NL1, NL2, TABLE).
raise_pos_1([], [], [], []).
raise_neg_1([A|As], [(+A1;-A)|NL1], [(+A1,-A)|NL2], [A-A1|TABLE]) :-
	genpred(A1),
	raise_neg_1(As, NL1, NL2, TABLE).
raise_neg_1([], [], [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% %% i.e. 1 -> 2
% n_elim_consistent(F, (G, F)) :-
% 	n_atomset(F, Sig),
% 	num_sig(Sig, 1, Sig1),
% 	num_sig(Sig, 2, Sig2),
% 	map_ec_impl(Sig1, Sig2, Conj),
% 	list_to_conjunction(Conj, G).
% 
% map_ec_impl([X|Xs], [Y|Ys], [(-X;+Y)|Xs1]) :-
% 	map_ec_impl(Xs, Ys, Xs1).
% map_ec_impl([], [], []).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- flag(genpred, _, 1).

reset_genpred :-
	flag(genpred, _, 1).

genpred(Symbol) :-
	flag_inc(genpred, Old),
	concat_atom([p, Old, '$'], Symbol).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Rename
%%%% 
%%%% 
%%%% Systematically rename predicates "FunctorN" to "FunctorM" where N, M is a
%%%% number. Number 0 as N or M stands for no number, i.e. just "Functor".
%%%% NMs is a list of N-M pairs, processed from left to right.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_rename(NMs, F, G) :-
	rename_1(NMs, F, G).

rename_1([NM|NMs], F, G) :-
	rename_spec(F, NM, ONs),
	replace_atoms(F, ONs, F1),
	rename_1(NMs, F1, G).
rename_1([], F, F).

rename_spec(F, N-M, OldNews) :-
	number(N),
	number(M),
	!,
	n_atomset(F, Sig),
	map_ren_spec(Sig, N, M, OldNews).
rename_spec(_, N-M, [N-M]).

ren_spec(From, N, M, From-To) :-
	( atom(N) -> N1 = N ; term_to_atom(N, N1) ),
	( atom(M) -> M1 = M ; term_to_atom(M, M1) ),
	From =.. [F|Args],
	atom_chars(F, F1),
	reverse(F1, [K|F2]),
	( char_type(K, digit) ->
	  F3 = F2,
	  K1 = K
	; F3 = [K|F2],
	  K1 = '0'
	),
	( K1=N1 ->
	  ( M1 = '0' ->
	    reverse(F3, F4)
	  ; reverse([M1|F3], F4)
	  ),
	  atom_chars(F5, F4),
	  To =.. [F5|Args]
	; To = From
	).
	
map_ren_spec([X|Xs], N, M, [X1|Xs1]) :-
	ren_spec(X, N, M, X1),
	map_ren_spec(Xs, N, M, Xs1).
map_ren_spec([], _, _, []).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


term_signature(T, S) :-
	term_signature_1(T, S1, []),
	sort(S1, S).

term_signature_1(T, S, S) :-
	var(T),
	!.
term_signature_1(T, [F/N|S], S1) :-
	functor(T, F, N),
	term_signature_2(T, N, S, S1).

term_signature_2(T, N, S, S1) :-
	N > 0,
	!,
	arg(N, T, TN),
	term_signature_1(TN, S, S2),
	N1 is N - 1,
	term_signature_2(T, N1, S2, S1).
term_signature_2(_, 0, S, S).

%%%% 
%%%% This could be made faster by handling propositional terms like 0-ary functions.
%%%% But the prop. signature could be extended by circumscription scopes.
%%%%
install_lrpo_for_formula(F) :-
	src_reference_signature(F, SPs, SFs),
	%% to really have the ordering on all possible terms in formulas,
	%% even those not considered by src_reference_signature are added
	term_signature(F, ST),
	append(SPs, ST, SX1),
	append(SFs, SX1, SX2),
	sort(SX2, S),
	LOGOPS = [(';')/2,(',')/2,(~)/1,forg/2,proj/2,
		  rename/2,raise/2,circ/2,'$heavy'/0],
	sort([true/0,false/0,(+)/1,(-)/1|LOGOPS], LOGOPS_S),
	ord_subtract(S, LOGOPS_S, S2),
	genpred_preds(100, SG),
	append(S2, SG, S3),
	sort(S3, S4),
	%% There might still be source level logops in S3 and probably
	%% other junk, but this is harmless
	append([true/0,false/0,(+)/1,(-)/1,'$light'/0| S4], LOGOPS, PREC),
	info(90, 'Installing lrpo for ~w', [PREC]),
	install_lrpo_ordering(PREC).


genpred_preds(0, []) :-
	!.
genpred_preds(N, [F/0|FNs]) :-
	N > 0,
	concat_atom([p, N, '$'], F),
	N1 is N - 1,
	genpred_preds(N1, FNs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Different ways of simplification are applied: rewriting respects lrpo
%%%% while others use the prolog term ordering. These should not be applied
%%%% intermingled to avoid non-termination.
%%%% 
%%%% Cheap simplifications are tried first.
%%%%

% n_simplify(F1, F2) :-
% 	rewrite_v2(simp, F1, F2).
% % 	rewrite(simp, F1, F2).



n_simplify(F1, F2) :-
	n_simp_tv_fast(F1, F3),
	n_simp_fast(F3, F4),
	( get_conf(simplifications, rough) ->
          F2 = F4
	; n_simplify_2(F4, F2)
	).

n_simplify_2_via_small_nf(F1, F2) :-
	n_f(F1, G1),
	cnf_prop(G1, G2),
	matrix_n(G2, F2),
 	n_size(F1, N0),
 	n_size(G2, N1),
 	N1 =< N0 + 8.

matrix_n([C], C1) :-
	!,
	clause_n(C, C1).
matrix_n([C|Cs], (C1,Cs1)) :-
	clause_n(C, C1),
	matrix_n(Cs, Cs1).
matrix_n([], true).

clause_n([~A], -A) :-
	!.
clause_n([A], +A) :-
	!.
clause_n([~A|Ls], (-A;Ls1)) :-
	!,
	clause_n(Ls, Ls1).
clause_n([A|Ls], (+A;Ls1)) :-
	!,
	clause_n(Ls, Ls1).
clause_n([], false).


n_simplify_rough(F1, F2) :-
 	n_simplify_conjunction(F1, F2),
 	!.
n_simplify_rough(F1, F2) :-
 	n_simplify_disjunction(F1, F2),
 	!.
n_simplify_rough(F1, F2) :-
	n_simp_tv_fast(F1, F3),
	n_simp_fast(F3, F2).

% % *** only for tests
% n_simplify_inner(F1, F2) :-
% 	!,
% 	rewrite_nx_simp(F1,F2).

%%%% 
%%%% n_simplify_inner is used "within" elimination etc. function
%%%% 
n_simplify_inner(F1, F2) :-
 	n_simp_tv_fast(F1, F3),
 	n_simp_fast(F3, F2).
%	n_simplify(F1, F2).

% n_simplify(F1, F2) :-
%  	n_unlink_all_if_cheap(F1, F3),
%  	rewrite(simp, F3, F2).


n_is_literal_list([+(_)|Xs]) :-
	!,
	n_is_literal_list(Xs).
n_is_literal_list([-(_)|Xs]) :-
	!,
	n_is_literal_list(Xs).
n_is_literal_list([true|Xs]) :-
	!,
	n_is_literal_list(Xs).
n_is_literal_list([]).

n_simplify_conjunction(F1, F2) :-
	conjunction_to_list(F1, L1),
	( memberchk(false, L1) ->
	  F2 = false
	; n_is_literal_list(L1),
	  sort(L1, L2),
	  ( L2 = [true|L3]
	  ; L3 = L2
	  ),
	  ( ordlits_negs(L3, Negs1),
	    switch_neglits(Negs1, Negs),
	    \+ ord_disjoint(Negs, L3) ->
	    F2 = false
	  ; list_to_conjunction(L3, F2)
	  )
	).

n_simplify_disjunction(F1, F2) :-
	disjunction_to_list(F1, L1),
	( memberchk(true, L1) ->
	  F2 = true
	; n_is_literal_list(L1),
	  sort(L1, L2),
	  ( L2 = [false|L3]
	  ; L3 = L2
	  ),
	  ( ordlits_negs(L3, Negs1),
	    switch_neglits(Negs1, Negs),
	    \+ ord_disjoint(Negs, L3) ->
	    F2 = true
	  ; list_to_disjunction(L3, F2)
	  )
	).

ordlits_negs([-X|Xs], [-X|Xs]) :-
	!.
ordlits_negs([_|Xs], Xs1) :-
	ordlits_negs(Xs, Xs1).
ordlits_negs([], []).

switch_neglits([-X|Xs], [+X|Xs1]) :-
	switch_neglits(Xs, Xs1).
switch_neglits([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic elim_macro/2.

define_elim_macro(S, T) :-
	retractall(elim_macro(S, _)),
	retractall((elim_macro(S, _) :- _)),
	( T = (T1 :- B) ->
	  E = (elim_macro(S, T1) :- B)
	; E = elim_macro(S, T)
	),
	assert(E).

expand_elim_macros(F, F1) :-
%	subexpression(F, S, F2, T),
	subexpression_inner_first(F, S, F2, T),
	\+ var(S),
	elim_macro(S, T),
	!,
	expand_elim_macros(F2, F1).
expand_elim_macros(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Syntactically determined - so it might be a proper superset of the
%%%% ebase. But simplifications can be applied in determining it. So it
%%%% might be a proper subset of the base.
%%%% 
xbase(F, S) :-
	reset_genpred,
	expand_elim_macros(F, F1),
	install_lrpo_for_formula(F1),
	src_to_nx(F1, F2),
	rewrite_nx_main(F2, F4),
	nx_scopetype(F4, S).

xbase_internal(F, S) :-
	expand_elim_macros(F, F1),
	src_to_nx(F1, F2),
	rewrite_nx_main(F2, F4),
	nx_scopetype(F4, S).

xabase(F, S) :-
	xbase(F, S1),
	map_lit_atom(S1, S2),
	sort(S2, S).

xabase_internal(F, S) :-
	xbase_internal(F, S1),
	map_lit_complement(S1, S2),
	sort(S2, S3),
	ord_union(S1, S3, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_simp_fast((false, _), false) :-
	!.
n_simp_fast((_, false), false) :-
	!.
n_simp_fast((true, F), F1) :-
	!,
	n_simp_fast(F, F1).
n_simp_fast((F, true), F1) :-
	!,
	n_simp_fast(F, F1).
n_simp_fast((F, F), F1) :-
	!,
	n_simp_fast(F, F1).
n_simp_fast((F, (F, G)), F1) :-
	!,
	n_simp_fast((F, G), F1).
n_simp_fast((F, G), (F1, G1)) :-
	!,
	n_simp_fast(F, F1),
	n_simp_fast(G, G1).


n_simp_fast((true; _), true) :-
	!.
n_simp_fast((_; true), true) :-
	!.
n_simp_fast((false; F), F1) :-
	!,
	n_simp_fast(F, F1).
n_simp_fast((F; false), F1) :-
	!,
	n_simp_fast(F, F1).
n_simp_fast((F; F), F1) :-
	!,
	n_simp_fast(F, F1).
n_simp_fast((F; (F; G)), F1) :-
	!,
	n_simp_fast((F; G), F1).
n_simp_fast((F; G), (F1; G1)) :-
	!,
	n_simp_fast(F, F1),
	n_simp_fast(G, G1).

n_simp_fast(F, F).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_simp_tv_fast((F,G), FG) :-
	!,
	( F = false -> FG = false
	; G = false -> FG = false
	; n_simp_tv_fast(F, F1),
	  ( F1 = false ->
	    FG = false
	  ; n_simp_tv_fast(G, G1),
	    ( G1 = false ->
	      FG = false
	    ; F1 = true ->
	      FG = G1
	    ; G1 = true ->
	      FG = F1
	    ; F1 = G1 ->
	      FG = F1
	    ; FG = (F1, G1)
	    )
	  )
	).
n_simp_tv_fast((F;G), FG) :-
	!,
	( F = true -> FG = true
	; G = true -> FG = true
	; n_simp_tv_fast(F, F1),
	  ( F1 = true ->
	    FG = true
	  ; n_simp_tv_fast(G, G1),
	    ( G1 = true ->
	      FG = true
	    ; F1 = false ->
	      FG = G1
	    ; G1 = false ->
	      FG = F1
	    ; F1 = G1 ->
	      FG = F1
	    ; FG = (F1; G1)
	    )
	  )
	).
n_simp_tv_fast(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% As: Atoms to forget either just positively, just negatively or
%%%%     in both polarities
%%%% Ws: literals (notation A or ~A) to keep (not to forget about)
%%%% 
%%%% This always succeeds.
%%%% 
n_litforg_args(S, F, As, Ws) :-
	n_scope(F, SF),
	sort(S, SP),
	scope_intersection(SF, SP, SForg),
	scope_atomsets_pos_neg_bi(SForg, AsPOS, AsNEG, AsBI),
	ord_union([AsPOS, AsNEG, AsBI], As),
	( setof(~A, (member(A, AsPOS), memberchk(-A, SF)), Ws1 ) ->
	  true
	; Ws1 = []
	),
	( setof(A, (member(A, AsNEG), memberchk(+A, SF)), Ws2 ) ->
	  true
	; Ws2 = []
	),
	ord_union(Ws1, Ws2, Ws).
	

%%%%
%%%% Used to check whether variable elimination techniques can be
%%%% applied that do not consider polarity.
%%%%
%%%% Fails if the input argument S does not represent an atom scope
%%%% with respect to the formula F. S does represent an atom scope
%%%% if for each of its members L it holds that the complement
%%%% of L is contained in S or the complement of L does not occur
%%%% in F (in the latter case, the literal forgetting is the same
%%%% as atom forgetting).
%%%%
%%%% Returns the atom scope of literals to forget as sorted list of
%%%% atoms.
%%%%
n_atom_scope(S, F, As) :-
	n_scope(F, SF),
	n_atom_scope_1(S, SF, As1),
	sort(As1, As).

n_atom_scope_1([], _, []).
n_atom_scope_1([L|S], SF, As) :-
	\+ memberchk(L, SF),
	!,
	n_atom_scope_1(S, SF, As).
n_atom_scope_1([+A|S], SF, [A|As]) :-
	( select(-A, S, S1) ->
	  true
	; \+ memberchk(-A, SF),
	  S1 = S
	),
	n_atom_scope_1(S1, SF, As).
n_atom_scope_1([-A|S], SF, [A|As]) :-
	( select(+A, S, S1) ->
	  true
	; \+ memberchk(+A, SF),
	  S1 = S
	),
	n_atom_scope_1(S1, SF, As).




%% cnf -- try
%%     -- force
%% def-cnf -- try elim, try def
%%         -- force elim, force def
%% 	-- force elim, try def
%% 	-- try elim, force def (unfold again?)


elim_via_dnf(AS, F, F1) :-
	dnf_prop(F, F2),
	!,
	via_dnf(AS, F2, F1).

via_dnf(AS, D, D1) :-
	map_via_dnf_c(D, AS, D2),
	red_subs(D2, D1).

via_dnf_c(D, AS, D1) :-
	findall(L, (member(L, D), mlit_atom(L, A), \+ memberchk(A, AS)), D1).

map_via_dnf_c([X|Xs], Y1, [X1|Xs1]) :-
	via_dnf_c(X, Y1, X1),
	map_via_dnf_c(Xs, Y1, Xs1).
map_via_dnf_c([], _, []).

elim_naive_rbr(AS, KeepLits, F, F1) :-
	cnf_prop(F, F2),
	naive_rbr(AS, KeepLits, F2, F1).
elim_naive_rbr_def(AS, KeepLits, F, F1) :-
 	df(F, D, L),
 	F2 = (L, D),
 	cnf_prop_fewsimp(F2, F4),
 	m_prop_signature(F4, Ps),
 	findall(P, (member(P, Ps), definitional_atom(P)), AS1),
 	append(AS, AS1, AS2),
 	sort(AS2, AS3),
 	naive_rbr(AS3, KeepLits, F4, F1).

sort_by_link_count(AS, F, AS1) :-
	map_link_count(AS, F, NAS2),
	keysort(NAS2, NAS3),
	map_val(NAS3, AS1).

map_link_count([X|Xs], Y1, [X1-X|Xs1]) :-
	link_count(X, Y1, X1),
	map_link_count(Xs, Y1, Xs1).
map_link_count([], _, []).

%%%% 
%%%% link_count is a heuristic value, smaller values to eliminate first
%%%% 
% link_count(A, F, N) :-
% 	findall(k, (member(C,F), memberchk(A,C)), POS),
% 	length(POS, NP),
% 	findall(k, (member(C,F), memberchk(~A,C)), NEG),
% 	length(NEG, NN),
% 	N is NP*NN.

link_count(A, F, N) :-
 	findall(k, (member(C,F), memberchk(A,C)), POS),
 	length(POS, NP),
 	findall(k, (member(C,F), memberchk(~A,C)), NEG),
 	length(NEG, NN),
	N1 is NP*NN,
	( N1 = 0 -> 
 	  N = 0
	; NP = 1, NN = 1 ->
	  N = 1
	; (NP = 1 ; NN = 1) ->
	  N is (2 + (1 - 1/(NP*NN)))
	; N is 3 + N1
	).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

map_key([X-_|Xs], [X|Xs1]) :-
	map_key(Xs, Xs1).
map_key([], []).

:- dynamic last_naive_rbr/2.

naive_rbr(Vs, W, M, M1) :-
%	retractall(last_naive_rbr(_, _)),
%	asserta(last_naive_rbr(Vs, M)),
	naive_rbr_1(Vs, W, 1, M, M1).

naive_rbr_1([V0|Vs0], W, K, M, M1) :-
	sort_by_link_count([V0|Vs0], M, [V|Vs]),
	length(Vs, TODO),
	length(M, MSIZE),
	info(60, 'naive_rbr: new round, vars:~| ~t~w~7+ size:~| ~t~w~7+', [TODO, MSIZE]),
	split_m(M, V, MP, MN, MR),
	( memberchk(V, W) ->
          KEEP = pos
	; memberchk(~V, W) ->
	  KEEP = neg
	; KEEP = none
	),
	findall(C, ( member(C1, MP),
		     member(C2, MN),
		     \+ ( member(L, C1),
			  mlit_complem(L, L1),
			  memberchk(L1, C2) ),
		     append(C1, C2, C3),
		     sort(C3, C)
		   ; member(C, MR)
		   ; KEEP = pos,
		     member(C4, MP),
		     C = [V|C4]
		   ; KEEP = neg,
		     member(C4, MN),
		     C = [~V|C4]
		   ),
		M2),
 	red_unit(M2, M3),
 	red_subs(M3, M4),
	K1 is K+1,
	naive_rbr_1(Vs, W, K1, M4, M1).
naive_rbr_1([], _, _, M, M).

split_m([C|Cs], V, [C1|Cs1], MN, MR) :-
	memberchk(V, C),
	!,
	once(select(V, C, C1)),
	split_m(Cs, V, Cs1, MN, MR).
split_m([C|Cs], V, MP, [C1|Cs1], MR) :-
	memberchk(~V, C),
	!,
	once(select(~V, C, C1)),
	split_m(Cs, V, MP, Cs1, MR).
split_m([C|Cs], V, MP, MN, [C|MR]) :-
	split_m(Cs, V, MP, MN, MR).
split_m([], _, [], [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_to_n(M, N) :-
	map_mc_to_n(M, M1),
	list_to_conjunction(M1, N).

mc_to_n(C, N) :-
	findall(L, (member(L1,C), (L1 = ~A -> L = -A ; L = +L1)), C1),
	list_to_disjunction(C1, N).

dnf_to_n(M, N) :-
	map_dnf_c_to_n(M, M1),
	list_to_disjunction(M1, N).

dnf_c_to_n(C, N) :-
	findall(L, (member(L1,C), (L1 = ~A -> L = -A ; L = +L1)), C1),
	list_to_conjunction(C1, N).

map_dnf_c_to_n([X|Xs], [X1|Xs1]) :-
	dnf_c_to_n(X, X1),
	map_dnf_c_to_n(Xs, Xs1).
map_dnf_c_to_n([], []).

map_mc_to_n([X|Xs], [X1|Xs1]) :-
	mc_to_n(X, X1),
	map_mc_to_n(Xs, Xs1).
map_mc_to_n([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_elim_via_dnf(S, F, F1) :-
	n_atom_scope(S, F, AS),
	!,
	n_f(F, F2),
	elim_via_dnf(AS, F2, F3),
	dnf_to_n(F3, F1).

n_elim_via_naive_rbr(S, F, F1) :-
	n_litforg_args(S, F, AS, WS),
	!,
	n_f(F, F2),
	elim_naive_rbr(AS, WS, F2, F3),
	m_to_n(F3, F1).

n_elim_via_naive_rbr_def(S, F, F1) :-
	n_litforg_args(S, F, AS, WS),
	!,
	n_f(F, F2),
	elim_naive_rbr_def(AS, WS, F2, F3),
	m_to_n(F3, F1).


%%%% 
%%%% CNF transformation by itself could expensive here.
%%%% 
elimination_task_to_cnf(tsk(A,B), CNF, WhiteList, BlackList) :-
	n_varelim_args(A, B, CNF, WhiteList, BlackList).
elimination_task_to_defcnf(tsk(A,B), CNF, WhiteList, BlackList) :-
	n_varelim_args_def(A, B, CNF, WhiteList, BlackList).

recorded_elimination_task(tsk(A,B)) :-
	rec(n_elim_lits(A, B, _)).


%%%%
%%%% n_varelim_args(+A, +B, -F, -White, -Black).
%%%%
%%%% Usage e.g.
%%%% :- rec(n_elim_lits(A,B,_)), n_varelim_args(A,B,F,White,Black).
%%%% 
%%%% The output F is a CNF that can be given as argument to
%%%% dimacsio:write_dimacs_file/3. White and Black are the whitelist
%%%% and blacklist, resp., as lists of propositional atoms.
%%%%
n_varelim_args(S, F, F1, White, Black) :-
	n_atom_scope(S, F, V),
	!,
	n_f(F, F2),
	cnf_prop(F2, F1),
	m_prop_signature(F1, FAs),
	ord_intersection(FAs, V, Black),
	ord_subtract(FAs, Black, White).

%%%% 
%%%% Same as n_varelim_args but with definitional CNF
%%%% 
n_varelim_args_def(S, F, F1, White, Black) :-	
	n_atom_scope(S, F, V),
	!,
	n_f(F, F2),
 	df(F2, D, L),
	F3 = (L, D),
 	cnf_prop_fewsimp(F3, F1),
 	m_prop_signature(F1, FAs),
 	( setof(P, (member(P, FAs), definitional_atom(P)), VD) ->
	  true
	; VD = []
	),
	ord_union(V, VD, V1),
	ord_intersection(FAs, V1, Black),
	ord_subtract(FAs, Black, White).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scheduling_spec_1(Scheduling, Methods, Lits, F, F1) :-
	Scheduling=m1,
	Methods = [method(0, 0.1, elim_dnf,
			  n_elim_via_dnf(Lits, F, F1)),
		   method(0, 20.0, elim_rbr,
			  n_elim_via_naive_rbr(Lits, F, F1)),
		   method(0, 0, elim_rew,
			  n_elim_via_rew(Lits, F, F1)),		 
		   method(20.0, 0, elim_def,
			  n_elim_via_naive_rbr_def(Lits, F, F1))].


scheduling_spec_1a(Scheduling, Methods, Lits, F, F1) :-
	get_conf(elim_method, rew_only),
	!,
	Scheduling=dummy,
	Methods = [method(0, 0, elim_rew,
			  n_elim_via_rew(Lits, F, F1))].
scheduling_spec_1a(Scheduling, Methods, Lits, F, F1) :-
	Scheduling=m1,
	Methods = [method(0, 0, elim_rew,
			  n_elim_via_rew(Lits, F, F1)),
		   method(0, 20.0, elim_rbr,
			  n_elim_via_naive_rbr(Lits, F, F1)),
		   method(20.0, 0, elim_def,
			  n_elim_via_naive_rbr_def(Lits, F, F1))].

n_elim_lits_1(Lits, F, F1) :-
	scheduling_spec_1(Scheduling, Methods, Lits, F, F1),
	schedule(Scheduling, Methods).

n_elim_lits_1a(Lits, F, F1) :-
	scheduling_spec_1a(Scheduling, Methods, Lits, F, F1),
	schedule(Scheduling, Methods).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_simplify_2_via_rew(F1, F2) :-
	rewrite_simp(F1, F2).

n_simplify_2(F1, F2) :-
	get_conf(time_limit(simp), MaxLimit),
	( schedule(m2(MaxLimit),
		   [method(0, 0, simp_con, n_simplify_conjunction(F1, F2)),
		    method(0, 0, simp_dis, n_simplify_disjunction(F1, F2)),
		    method(0, 0.05, simp_cnf,
			   n_simplify_2_via_small_nf(F1, F2)),
		    method(0,MaxLimit,simp_rew,
			   n_simplify_2_via_rew(F1, F2))]) ->
	  true
	; info(20, 'skipping simplifications that would take too long'),
          ( get_conf(record_mode, true) ->
	    asserta(rec(n_simplify(F1, _)))
	  ; true
	  ),
	  F2 = F1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Scheduling Methods for a Task with Incremental Time Limits
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

schedule(IncMode, Methods) :-
	schedule(IncMode, 0, Methods, []).
schedule(dummy, _, [M], []) :-
	!,
	M = method(_, _, _, Call),
	call(Call).
schedule(IncMode, N, [M|Ms], Ms1) :-
	step_val(IncMode, N, V),
	M = method(TMin, TMax, Name, Call),
	( %% once the Limit is larger than TMax we remove the method
	  V > TMax, TMax \= 0 ->
	  schedule(IncMode, N, Ms, Ms1)
	; V < TMin ->
	  schedule(IncMode, N, Ms, [M|Ms1])
	; info(40, 'Trying scheduled method:     ~w ~w', [Name, V]),
	  catch(call_with_time_limit(V, Call),
		time_limit_exceeded, Exceeded = true) ->
	  ( Exceeded == true ->
	    info(40, 'Failure of scheduled method: ~w ~w', [Name, V]),
            schedule(IncMode, N, Ms, [M|Ms1])
	  ; info(40, 'Success of scheduled method: ~w ~w', [Name, V]),
	    true
	  )
	; %% method failed, it does not make sense to try it
	  %% with larger timeouts
	  schedule(IncMode, N, Ms, Ms1)
	).
schedule(IncMode, N, [], Ms) :-
	Ms \= [],
	N1 is N + 1,
	schedule(IncMode, N1, Ms, []).

step_val(m1, N, Val) :-
	( N =< 2 -> 
	  Val is 0.05 + N * 0.05
	; N =< 7 ->
	  Val is 0.35 + (N - 3) * 0.2
	; Val is 1.65 + (N - 7) * 0.5
	).

step_val(m3, N, Val) :-
	K = 0.01,
	E = 4,
	Val is K + K * N^E.


step_val(m2(_), 0, 0.05).
step_val(m2(X), N, X) :-
	N > 0.


enum_n(0).
enum_n(N) :-
	enum_n(N1),
	N is N1 + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Conversion of Scope Specifiers to Scopes as Ordsets of Literals
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scopespec_scope_intersect(complements(Spec), S, S1) :-
	!,
	scopespec_complements(Spec, Spec1),
	scopespec_scope_intersect(Spec1, S, S1).
scopespec_scope_intersect(Spec, S, S1) :-
	( setof(A, (member(+A, S) ; member(-A, S)), Sig ) ->
	  true
	; Sig = []
	),
	%% We use the set of atoms here, since we use the predicates from
	%% canonic_scope_delimited. This could be implemented more directly
	%% with literal scopes.
	expand_scope_delimited(Spec, Sig, S2),
	sort(S2, S3),
	ord_intersection(S3, S, S1).

scopespec_expand(complements(Spec), RefPs, RefFs, S1) :-
	!,
	scopespec_complements(Spec, Spec1),
	scopespec_expand(Spec1, RefPs, RefFs, S1).
scopespec_expand(Spec, RefPs, RefFs, S1) :-
	expand_scope_full(Spec, RefPs, RefFs, S2),
	sort(S2, S1).

scopespec_union(S1, S2, S3) :-
	append(S1, S2, S3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Scopes Represented as Ordsets of Literals
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

linkless_inside(SF, SG, S) :-
	scopes_halflinks(SF, SG, SL), 
	scope_complements(S, S1),
	scope_union(S, S1, S2),
	scope_disjoint(SL, S2).

% scopes_enum_atoms_linked_inside(SF, SG, S, A) :-
% 	scopes_halflinks(SF, SG, SL),
% 	scope_complements(S, SC),
% 	scope_union(S, SC, S1),
% 	scope_intersection(SL, S1, SL1),
% 	scope_atomset(SL1, As),
% 	member(A, As).

scopes_enum_atoms_linked_inside(SF, SG, S, A) :-
	scope_atomset(S, As),
	member(A, As),
	( scope_memberchk(+A, SF),
	  scope_memberchk(-A, SG) ->
	  true
	; scope_memberchk(-A, SF),
	  scope_memberchk(+A, SG)
	).

scopes_halflinks(S1, S2, SL) :-
	scope_complements(S2, S3),
	scope_intersection(S1, S3, SL).

scope_biscope(S, S1) :-
	scope_uniscope(S, S2),
	scope_subtract(S, S2, S1).

scope_uniscope(S, S1) :-
	scope_complements(S, S2),
	scope_subtract(S, S2, S1).

scope_subtract(S1, S2, S) :-
	ord_subtract(S1, S2, S).

scope_subseteq(S1, S2) :-
	ord_subset(S1, S2).

scope_intersection(S1, S2, S) :-
	ord_intersection(S1, S2, S).

scope_disjoint(S1, S2) :-
	ord_disjoint(S1, S2).

scope_union(S1, S2, S) :-
	ord_union(S1, S2, S).

scope_union(Ss, S) :-
	ord_union(Ss, S).

scope_complements(S, S1) :-
	map_lit_complement(S, S2),
	sort(S2, S1).

scope_memberchk(L, S) :-
	ord_memberchk(L, S).

scope_cup_complements(S, S1) :-
	scope_complements(S, S2),
	scope_union(S, S2, S1).

scope_is_atom_scope(S) :-
	scope_complements(S, S1),
	scope_equal(S, S1).

scope_equal(S, S).

%%%%
%%%% Break a forgetting scope into two parts:
%%%%
%%%% +S    : Forgetting scope
%%%% +SF   : Formula scope
%%%% -X    : Set of atoms for atom forgetting
%%%% -SLit : Set of literals for literal forgetting
%%%% 
scope_atom_and_lit_scopes(S, SF, X, SLit) :-
	scope_intersection(S, SF, S1),
	scope_complements(S1, S1C),
	scope_subtract(S1C, S1, S2C),
	scope_intersection(S2C, SF, SLitC),
	scope_complements(SLitC, SLit),
	scope_subtract(S1, SLit, SAto),
	scope_atomset(SAto, X).
	
scope_is_atom_scope(S, SF, X) :-
	%%
	%% For each member of (S cap SF) it holds that:
	%% if its complement is in SF then this complement is also in S
	%%
	scope_intersection(S, SF, S1),
	scope_complements(S1, S1C),
	scope_intersection(S1C, SF, S2),
	scope_subseteq(S2, S1),
	!,
	map_lit_atom(S1, X1),
	sort(X1, X).


scope_atomset(S, As) :-
	sca_1(S, As1),
	sort(As1, As).

sca_1([+A|Ls], [A|As]) :-
	sca_1(Ls, As).
sca_1([-A|Ls], [A|As]) :-
	sca_1(Ls, As).
sca_1([], []).

scope_atomsets_pos_neg_bi(S, AsPOS, AsNEG, AsBI) :-
	( setof(A1, member(+A1, S), Pos1) -> true ; Pos1 = [] ),
	( setof(A2, member(-A2, S), Neg1) -> true ; Neg1 = [] ),
	ord_subtract(Pos1, Neg1, AsPOS),
	ord_subtract(Neg1, Pos1, AsNEG),
	ord_intersection(Pos1, Neg1, AsBI).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rename_literal(RS, +A, +B) :-
	rename_atom(RS, A, B).
rename_literal(RS, -A, -B) :-
	rename_atom(RS, A, B).

rename_atom([X-Y|XYs], A, A1) :-
	!,
	rename_match(X, Y, A, A2),
	rename_atom(XYs, A2, A1).
rename_atom([], A, A).
	
rename_match(X, Y, A, B) :-
	copy_term(X-Y, X1-Y1),
	( number(X1) ->
	  ( get_group(A, X1) ->
	    ( number(Y1) ->
	      set_group(A, Y1, B)
	    ; B = Y1
	    )
	  ; B = A
	  )
	; A = X1 ->
	  ( number(Y1) ->
	    set_group(A, Y1, B)
	  ; B = Y1
	  )
	; B = A
	).
	  
rename_scope(S, RS, S1) :-
	map_rename_literal(S, RS, S2),
	sort(S2, S1).

map_rename_literal([X|Xs], Y1, [X1|Xs1]) :-
	rename_literal(Y1, X, X1),
	map_rename_literal(Xs, Y1, Xs1).
map_rename_literal([], _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Support for Interruptable Methods
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_val(Key, Val) :-
	%%
	%% we use setup_call_cleanup to ensure tha this is atomic w.r.t.
	%% with_time_limit.
	%%
	setup_call_cleanup((( recorded(Key, _, Ref),
			      erase(Ref),
			      fail
			    ; true
			    ),
			    recorda(Key, Val)),
			   true,
			   true).
get_val(Key, Val) :-
	recorded(Key, Val).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% NX MAIN SIMPLIFICATION
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Includes: distribution of forg over disjunctions [and maybe linkless
%%%% conjunctions], evaluation of rename
%%%% 

rewrite_nx_main(F, F1) :-
	( get_conf(simp_main_method, 102) ->
	  rewrite_nx_v2(F, F1)
	; get_conf(simp_main_method, 101) ->
	  rewrite_nx_v1(F, F1)
	; get_conf(simp_main_method, 103) ->
	  rw_main_v1_until_fix(F, F1)
	; get_conf(simp_main_method, 104) ->
	  rw_main_v1(F, F1)
	; rw_main_v1_until_fix(F, F1)
	).

rewrite_nx_v1(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_main(S, T),
	!,
	rewrite_nx_v1(F2, F1).
rewrite_nx_v1(F, F).

rewrite_nx_v2(F, F1) :-
 	subformula_nx(F, S, F2, T),
 	nx_main(S, T1),
 	!,
 	rewrite_nx_v2_sub(T1, T),
 	rewrite_nx_v2(F2, F1).
rewrite_nx_v2(F, F).

rewrite_nx_v2_sub(F, F1) :-
 	subformula_nx(F, S, F2, T),
 	nx_main(S, T1),
 	!,
 	rewrite_nx_v2_sub(T1, T),
 	rewrite_nx_v2_sub(F2, F1).
rewrite_nx_v2_sub(F, F).

nx_main(~((F, G)), (~F; ~G)).
nx_main(~((F; G)), (~F, ~G)).
nx_main(~(~(F)), F).
nx_main(~(true), false).
nx_main(~(false), true).
nx_main(~(+A), -A).
nx_main(~(-A), +A).

nx_main((true; _), true).
nx_main((_; true), true).
nx_main((false, _), false).
nx_main((_, false), false).
nx_main((true, F), F).
nx_main((F, true), F).
nx_main((false; F), F).
nx_main((F; false), F).

nx_main(rename(_, true), true).
nx_main(rename(_, false), false).
nx_main(rename(RS, (F , G)), (rename(RS, F) , rename(RS, G))).
nx_main(rename(RS, (F ; G)), (rename(RS, F) ; rename(RS, G))).
nx_main(rename(RS, ~(F)), ~rename(RS, F)).
nx_main(rename(RS1, rename(RS2, F)), rename(RS, F)) :-
	append(RS2, RS1, RS).
nx_main(rename(RS, +A), +B) :-
	rename_atom(RS, A, B).
nx_main(rename(RS, -A), -B) :-
	rename_atom(RS, A, B).

nx_main(forg([], F), F).
nx_main(forg(_, true), true).
nx_main(forg(_, false), false).
nx_main(forg(S1, forg(S2, F)), forg(S12, F)) :-
 	scope_union(S1, S2, S12).
nx_main(forg(S, (F ; G)), (forg(S, F) ; forg(S, G))).

% % %%%% ***
% nx_main(forg(S, F), F1) :-
%    	nx_dec_step(S, F, F1).
% %	writeln(dc(F1)).

% nx_main(forg(S, (F , G)), (forg(S, F) , forg(S, G))) :-
%   	nx_scopetype(F, SF),
%   	nx_scopetype(G, SG),
%   	linkless_inside(SF, SG, S).

% Thu Apr 18 21:46:08 2013 - commented out because possibly bugged
% nx_main(forg(S, (F , G)), F1) :-
%   	dec_binary_inward(S, F, G, F1).

% nx_main(forg(S, F), forg(S1, F)) :-
%  	nx_scopetype(F, SF),
% 	scope_intersection(S, SF, S1),
% 	S1 \= S.
nx_main(forg(S, F), F) :-
 	nx_scopetype(F, SF),
 	scope_disjoint(S, SF).
nx_main(forg(S, +A), +A) :-
	\+ scope_memberchk(+A, S).
nx_main(forg(S, -A), -A) :-
	\+ scope_memberchk(-A, S).
nx_main(forg(S, +A), true) :-
	scope_memberchk(+A, S).
nx_main(forg(S, -A), true) :-
	scope_memberchk(-A, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% ELIM (NEW VERSION)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elim_new(F, X) :-
	elim(F, X, form).

elim_d(F, X) :-
	elim(F, X, dnf).

elim(F, X, ApproxOutputFormat) :-
	fix_random,
	reset_genpred,
	info(70, 'Expanding elim macros'),
	expand_elim_macros(F, F1),
	info(70, 'Installing lrpo'),
	install_lrpo_for_formula(F1),
	info(70, 'Preparing'),
	src_to_nx(F1, F2),
	info(70, 'Preparing: rewriting 1'),
	rewrite_nx_main(F2, F4),
	info(70, 'Entering loop'),
	nx_loop_3(F4, F7),
	nx_size(F7, N7),
	info(70, 'Simplifying the result, size: ~| ~t~w~7+', [N7]),
	( ApproxOutputFormat = dnf ->
	  info(70, 'Simplifying method: dnf'),
	  n_f(F7, F71),
	  dnf_prop(F71, F81),
	  dnf_to_n(F81, F8)
	; info(70, 'Simplifying method: nx simp'),
	  rewrite_nx_main(F7, F8)
	),
	nx_size(F8, N8),
	info(70, 'Simplifying done,       size: ~| ~t~w~7+', [N8]),
	( \+ nx_is_plain(F8) ->
	  pp(F8),
	  err('Result not plain')	    
	; true
	),
	nx_to_src(F8, X1),
	X = X1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Main Loops
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% These loops should return main-simplified formulas
%%%%

%%%%
%%%% A loop without any external solver. This can be used as fallback..
%%%% Currently just uses n_elim_via_rew as elimination method.
%%%%
nx_loop_1(F0, F1) :-
	nx_simp_unlink(F0, F),
	nx_size(F, N),
	info(60, 'unlink loop: size: ~| ~t~w~7+', [N]),
	( subformula_nx(F, S, F2, T),
	  S = forg(FS, FF),
	  nx_is_plain(FF) ->
	  n_elim_via_rew(FS, FF, T),
	  simp_nx_loop_1(F2, F3),
	  nx_loop_1(F3, F1)
	; F1 = F
	).

%%%% 
%%%% For testing purposes
%%%% 
nx_loop_8(F0, F1) :-
	nx_simp_unlink(F0, F),
	nx_size(F, N),
	info(60, 'unlink loop: size: ~| ~t~w~7+', [N]),
	( subformula_nx(F, S, F2, T),
	  S = forg(FS, FF),
	  nx_is_plain(FF) ->
	  n_elim_via_naive_rbr(FS, FF, T),
	  simp_nx_loop_1(F2, F3),
	  nx_loop_8(F3, F1)
	; F1 = F
	).


%%%% 
%%%% Like loop_1, but uses a portfolio of Prolog implementations
%%%% for literal elimination.
%%%%
nx_loop_1a(F0, F1) :-
	nx_simp_unlink(F0, F),
	nx_size(F, N),
	info(60, 'unlink loop: size: ~| ~t~w~7+', [N]),
	( subformula_nx(F, S, F2, T),
	  S = forg(FS, FF),
	  nx_is_plain(FF) ->
	  n_elim_lits_1a(FS, FF, T),
	  simp_nx_loop_1(F2, F3) ->  %% *** 2015 added "->" ???
	  nx_loop_1a(F3, F1)
	; F1 = F
	).



%%%% 
%%%% For debugging, like nx_loop_1 but shows subtasks
%%%% 
nx_loop_2(F0, F1) :-
	nx_simp_unlink(F0, F),
	nx_size(F, N),
	info(60, 'unlink loop: size: ~| ~t~w~7+', [N]),
	show_subtasks(F),
	( subformula_nx(F, S, F2, T),
	  S = forg(FS, FF),
	  nx_is_plain(FF) ->
	  n_elim_via_rew(FS, FF, T),
	  simp_nx_loop_1(F2, F3),
	  nx_loop_2(F3, F1)
	; F1 = F
	).

show_subtasks(F) :-
	writeln('==== subtasks'),
	( subformula_nx(F, S, _, _),
	  subtask(Type, S, ST),
	  writeln(Type-ST),
	  fail
	; true
	).


%%%% 
%%%% Defers subtasks to solve_subtask
%%%% 
nx_loop_3(F0, F1) :-
	nx_simp_unlink(F0, F),
	nx_size(F, N),
	info(10, 'global loop: size: ~| ~t~w~7+', [N]),
	( (Key = sat; Key = qbf ; Key = elim),
	  subformula_nx(F, S, F2, T),
	  subtask(Key, S, ST)  ->
	  nx_size(S, NS),
	  ( ST = task_elim(Vs, Ls, _) ->
	    length(Vs, NQV),
	    length(Ls, NQL),
	    NQ is NQV + NQL
	  ; NQ = 0
	  ),
	  info(15, 'subtask                     ~w formula-size: ~| ~t~w~7+  scope-size: ~| ~t~w~7+', [Key, NS, NQ]),
	  solve_subtask(ST, Key, S, T),
	  simp_nx_loop_1(F2, F3),
	  nx_loop_3(F3, F1)
	; F1 = F
	).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Unlinking as Simplification
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


nx_simp_unlink(F, F) :-
	get_conf(simp_aux_method, 2),
	!.
%%%% 
%%%% Returns a main-simplified formula
%%%% 
nx_simp_unlink(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_enum_unlink_inside_forg(S, _, T),
	nx_size(S, NS),
	nx_size(T, NT),
 	NT =< NS,
	simp_nx_simp_unlink(F2, F3),
	nx_size(F, NF),
	nx_size(F3, NF3),
	NF3 < NF,
	!,
	info(60, 'simp unlink size: ~| ~t~w~7+ to ~| ~t~w~7+', [NF, NF3]),
	nx_simp_unlink(F3, F1).
nx_simp_unlink(F, F).

nx_enum_unlink_inside_forg(forg(S, F), Upon, forg(S, F1)) :-
	nx_is_plain(F),
	%%
	%% both is possible here: _nx or  _inner_first_nx
	%%
%	subformula_inner_first_nx(F, FS, F1, FT),
	subformula_nx(F, FS, F1, FT),
	nx_enum_unlink_conj(FS, S, Upon, FT).
	
nx_enum_unlink_conj((F,G), S, Upon, F1) :-
	%%
	%% Nonplain (F,G) might be difficult here, since then no progress
	%% might be made by unlinking. So the caller probably should
	%% ensure that (F,G) is plain.
	%%
	nx_scopetype(F, SF),
	nx_scopetype(G, SG),
	scopes_enum_atoms_linked_inside(SF, SG, S, Upon),
	F2 = ((+Upon, (F , G)) ; (-Upon, (F , G))),
	simp_nx_unlink_subform(F2, F1).

% nx_enum_unlink_conj(F, S, Upon, F1) :-
% 	conjunction_to_list(F, Fs),
% 	Fs = [_,_|_],
% 	map_add_scopetype(Fs, FSs),
% 	select(FA-SA, FSs, FSs1),
% 	select(FB-SB, FSs1, FSs2),
% 	scopes_enum_atoms_linked_inside(SA, SB, S, Upon),
% 	F2 = ((+Upon, (FA , FB)) ; (-Upon, (FA , FB))),
% 	map_key(FSs2, Fs2),
% 	list_to_conjunction(Fs2, F3),
% 	F4 = (F2, F3),
% 	simp_nx_unlink_subform(F4, F1).

map_add_scopetype([X|Xs], [X-S|Xs1]) :-
	nx_scopetype(X, S),
	map_add_scopetype(Xs, Xs1).
map_add_scopetype([], []).

simp_nx_loop_1(F, F1) :-
 	rewrite_nx_main(F, F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simp_nx_simp_unlink(F, F1) :-
	simp_nx_unit_1(F, F2),
	rewrite_nx_main(F2, F1).

simp_nx_unlink_subform(F, F1) :-
	simp_nx_unit_1(F, F2),
   	rewrite_nx_main(F2, F1).

simp_nx_unit_1(F, F1) :-
	%%
	%% CHECK with experiments: unit is with lrpo before seems slow, but
	%% this seems fastest.
	%%
	rewrite_nx_rule_unitpre(F, F2),
    	nx_unit(F2, [], _, F1).

% simp_nx_unit_1(F, F1) :-
%     	nx_unit(F, [], _, F1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use_external_sat_solver :-
	get_conf(default_external_sat_solver, S),
	S \= none.

use_external_qbf_solver :-
	get_conf(default_external_qbf_solver, S),
	S \= none.

use_external_elim_solver :-
	get_conf(default_external_elim_solver, S),
	S \= none.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Solve QBF
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_task_qbf(_, true, _, true) :-
	!.
solve_task_qbf(_, false, _, false) :-
	!.
solve_task_qbf([], F, O, Value) :-
	!,
	solve_task_sat(F, O, Value).
solve_task_qbf([ex(_)], F, O, Value) :-
	!,
	solve_task_sat(F, O, Value).
solve_task_qbf([all(_)], F, O, Value) :-
	!,
	n_negate(F, F1),
	solve_task_sat(F1, O, Value1),
	( Value1 = true ->
	  Value = false
	; Value1 = false ->
	  Value = true
	).
solve_task_qbf(Q, F, Options,  Value) :-
	use_external_qbf_solver,
	external_solve(F, [qprefix-Q|Options], Solution),
	( Solution == satisfiable ->
	  Value = true
	; Solution == unsatisfiable ->
	  Value = false
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SOLVE SAT
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_task_sat(true, _, true) :-
	!.
solve_task_sat(false, _, false) :-
	!.
solve_task_sat(+_, _, true) :-
	!.
solve_task_sat(-_, _, true) :-
	!.
solve_task_sat(F, Options,  Value) :-
	use_external_sat_solver,
	external_solve(F, Options, Solution),
	( Solution == satisfiable ->
	  Value = true
	; Solution == unsatisfiable ->
	  Value = false
	).

external_solve(F0, Options, Solution) :-
	external_solve(F0, Options, Solution, _).

external_solve(F0, Options, Solution, OutForm) :-
	nx_to_src(F0, F),
	( memberchk(qprefix-QPrefix, Options), QPrefix \= [] ->
	  MODE = qbf
	; ( memberchk(elim-Elim0, Options) ->
	    MODE = elim,
	    sort(Elim0, Elim)
	  ; MODE = sat
          )
	),
	MinSolverTimeout = 0.5,
	get_conf(time_limit(cnf_nondef), MaxNodefCNFTimeout),
	get_conf(time_limit(cnf_nondef), DefaultNondefCNFTimeout),
	( memberchk(time_limit-TimeLimit, Options) ->
	  SolverTimeout is max(MinSolverTimeout, TimeLimit / 2.0),
	  CNFTimeout is TimeLimit / 4.0,
	  _NondefCNFTimeout is min(MaxNodefCNFTimeout, CNFTimeout / 4.0)  
	; get_conf(time_limit(external_solvers), SolverTimeout),
	  CNFTimeout = 0,
          _NondefCNFTimeout = DefaultNondefCNFTimeout
	),
	( fail %% directly use definitional CNF
% 	  with_time_limit(_NondefCNFTimeout, cnf_prop(F, M)) ->
%  	  QPrefix1  = QPrefix,
%  	  Elim1 = Elim,
%  	  ( MODE = elim -> m_prop_signature(M, MSig) ; true )
	; df(F, D, L),
	  F1 = (L, D),
	  ( CNFTimeout = 0 ->
	    cnf_prop_nosimp(F1, M)
	  ; with_time_limit(CNFTimeout, cnf_prop_nosimp(F1, M))
	  ),
	  ( (MODE = qbf ; MODE = elim) ->
            m_prop_signature(M, MSig),
 	    ( setof(P, (member(P, MSig), definitional_atom(P)), DefAtoms) ->
	      true
	    ; DefAtoms = []
	    ),
	    ( MODE = qbf ->
	      append(QPrefix, [ex(DefAtoms)], QPrefix1)
	    ; MODE = elim ->
	      ord_union(Elim, DefAtoms, Elim1)
	    )
	  ; true
	  )
	),
	( M = [] ->
	  Solution = satisfiable,
	  OutForm = true
	; M = [[]] ->
	  Solution = unsatisfiable,
	  OutForm = false
	; MODE = sat,
	  external_solve_matrix(M,
				[timeout-SolverTimeout, solution-Solution])
	; MODE = qbf,
	  external_solve_matrix(M,
				[qprefix-QPrefix1,
				 timeout-SolverTimeout,
				 solution-Solution])
	; MODE = elim,
	  ord_intersection(MSig, Elim1, Black),
	  ord_subtract(MSig, Black, White),
	  external_solve_matrix(M,
				[white-White,
				 black-Black,
				 timeout-SolverTimeout,
				 solution-Solution,
				 out_cnf-OutCNF,
				 out_dnf-OutDNF]),
	  ( nonvar(OutCNF) ->
	    m_to_n(OutCNF, OutForm)
	  ; nonvar(OutDNF) ->
	    dnf_to_n(OutDNF, OutForm)
	  ; fail
	  )
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SOLVE ELIM
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_task_varelim([], F, _, F) :-
	!.
solve_task_varelim(_, true, _, true) :-
	!.
solve_task_varelim(_, false, _, false) :-
	!.
solve_task_varelim(Vs, F, Options, Value) :-
	use_external_elim_solver,
	get_conf(debug_external_elim_solver, true),
	!,
	( solve_task_varelim_debug(Vs, F, Options, Value) ->
	  true
	; info(4, 'DEBUG: WARNING - Call failed')
	).
solve_task_varelim(Vs, F, Options, Value) :-
	use_external_elim_solver,
	external_solve(F, [elim-Vs|Options], _, Value).

solve_task_varelim_debug(Vs, F, Options, Value) :-
 	info(4, 'Debug: Calling external elim solver'),
 	time(external_solve(F, [elim-Vs|Options], _, Value)),
 	info(4, 'Debug: Calling naive rbr elim solver'),
 	findall(L, (member(V,Vs), (L = +V; L= -V)), S),
 	time(n_elim_via_naive_rbr(S, F, ValueR)),
 	info(4, 'Debug: Comparing external and fallback results'),
 	n_f(ValueR, ValueF),
 	n_f(Value, ValueF1),
	external_solve_form(~((ValueF <-> ValueF1)), [solution-Solution]),
	( Solution == unsatisfiable ->
	  info(4, 'Debug: COMPARE: External and fallback results coincide')
        ; info(4, 'Debug: COMPARE - WARNING: External and fallback results differ'),
	  asserta(te_fail(task_varelim(Vs, F), Vs, ValueF, ValueF1))
        ).
%  	cnf_prop(ValueF1, ValueF2),
%  	m_to_n(ValueF2, Value2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_subtask(task_elim(Vs,Ls,F), elim, Src, Value) :-
	!,
	convert_elim_to_varelim(Vs, Ls, F, Src, Vs1, F1, Src1),
% 	writeq(examp(solve_subtask(task_varelim(Vs1,F1), elim, Src1, Value))),
% 	writeln('.'),
% 	nl,
	solve_subtask(task_varelim(Vs1,F1), elim, Src1, Value).
solve_subtask(Tsk, Key, Src, Value) :-
	gensym(tsk, Sym),
	( get_conf(record_mode, true) ->
	  asserta(rec(solve_subtask(Tsk, Key, Src, Value))),
	  statistics(cputime, TBefore)
	; true
	),
	( info(20, 'Solving ~w task ~w', [Key, Sym]),
	  solve_subtask_1(Tsk, Key, Src, Value) ->
	  true
	; info(20, 'Using fallback method for ~w task ~w', [Key, Sym]),
	  fallback_solve(Src, Value)
	),
	( get_conf(record_mode, true) ->
	  statistics(cputime, TAfter),
	  T is TAfter - TBefore,
	  asserta(rec(time(T, solve_subtask(Tsk, Key, Src, Value))))
	; true
	).



%%%% 
%%%% solve_subtask_1 is allowed to fail for whatever reason (e.g. timeout).
%%%% 
solve_subtask_1(task_sat(F), _, _, Value) :-
	solve_task_sat(F, [], Value).
solve_subtask_1(task_qbf(Q, F), _, _, Value) :-
	solve_task_qbf(Q, F, [], Value).
solve_subtask_1(task_varelim(Vs, F), _, _, Value) :-
	Options = [],
	solve_task_varelim(Vs, F, Options, Value).


fallback_solve(F, F1) :-
%%	nx_loop_1(F, F1). %% OLD
	nx_loop_1a(F, F1). %% used
%	nx_loop_8(F, F1). %% test


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Identify SAT, QBF, ELIM as subtasks
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subtask(sat, forg(S, F), task_sat(F)) :-
	nx_is_plain(F),
	nx_scopetype(F, SF),
	scope_subseteq(SF, S).

subtask(qbf, forg(S, F), task_qbf(Q, F1)) :-
	nx_scopetype(F, SF),
	scope_subseteq(SF, S),
	is_qbf(F, Q, F1).

subtask(elim, forg(S, F), task_elim(AS, LS, F)) :-
	nx_is_plain(F),
	nx_scopetype(F, SF),
	scope_atom_and_lit_scopes(S, SF, AS, LS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Auxiliary Functions for Subtask Identification
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_qbf(F, Q, F1) :-
	is_qbf_pos(F, [], Q1, F1),
	n_atomset(F1, SIG),
	cleanup_quants(Q1, [], SIG, Q).


is_qbf_pos((F , G), Vs, Q, (F1 , G1)) :-
	!,
	is_qbf_pos(F, Vs, Q1, F1),
	is_qbf_pos(G, Vs, Q2, G1),
	append(Q1, Q2, Q).
is_qbf_pos((F ; G), Vs, Q, (F1 ; G1)) :-
	!,
	is_qbf_pos(F, Vs, Q1, F1),
	is_qbf_pos(G, Vs, Q2, G1),
	append(Q1, Q2, Q).
is_qbf_pos(true, _, [], true) :-
	!.
is_qbf_pos(false, _, [], false) :-
	!.
is_qbf_pos(+F, Vs, [], +F1) :-
	!,
	vs_lookup(F, Vs, F1).
is_qbf_pos(-F, Vs, [], -F1) :-
	!,
	vs_lookup(F, Vs, F1).
is_qbf_pos(forg(S, F), Vs, [ex(X1)|Q], F1) :-
	nx_scopetype(F, SF),
	scope_is_atom_scope(S, SF, X),
	add_vs(X, Vs, X1, Vs1),
	is_qbf_pos(F, Vs1, Q, F1),
	!.
is_qbf_pos(~(forg(S, F)), Vs, [all(X1)|Q], F1) :-
	nx_scopetype(F, SF),
	scope_is_atom_scope(S, SF, X),
	add_vs(X, Vs, X1, Vs1),
	is_qbf_neg(F, Vs1, Q, F1),
	!.
is_qbf_neg((F , G), Vs, Q, (F1 ; G1)) :-
	!,
	is_qbf_neg(F, Vs, Q1, F1),
	is_qbf_neg(G, Vs, Q2, G1),
	append(Q1, Q2, Q).
is_qbf_neg((F ; G), Vs, Q, (F1 , G1)) :-
	!,
	is_qbf_neg(F, Vs, Q1, F1),
	is_qbf_neg(G, Vs, Q2, G1),
	append(Q1, Q2, Q).
is_qbf_neg(true, _, [], false) :-
	!.
is_qbf_neg(false, _, [], true) :-
	!.
is_qbf_neg(+F, Vs, [], -F1) :-
	!,
	vs_lookup(F, Vs, F1).
is_qbf_neg(-F, Vs, [], +F1) :-
	!,
	vs_lookup(F, Vs, F1).
is_qbf_neg(forg(S, F), Vs, [all(X1)|Q], F1) :-
	nx_scopetype(F, SF),
	scope_is_atom_scope(S, SF, X),
	add_vs(X, Vs, X1, Vs1),
	is_qbf_neg(F, Vs1, Q, F1),
	!.
is_qbf_neg(~(forg(S, F)), Vs, [ex(X1)|Q], F1) :-
	nx_scopetype(F, SF),
	scope_is_atom_scope(S, SF, X),
	add_vs(X, Vs, X1, Vs1),
	is_qbf_pos(F, Vs1, Q, F1),
	!.

add_vs([X|_], Vs, _, _) :-
	%%
	%% Nested occurences of forgetting the sam atom might be subtle with
	%% variable renaming & literal forgetting. So we just fail in this
	%% case. (In some cases it might be handled by the "scope inference",
	%% a variable forgotten in an inner expression does not need to
	%% be forgotten on the outer level).
	%%
	memberchk(X-_, Vs),
	!,
	fail.
add_vs([X|Xs], Vs, [Y|Ys], [X-Y|Vs1]) :-
	genpred(Y),
	add_vs(Xs, Vs, Ys, Vs1).
add_vs([], Vs, [], Vs).

vs_lookup(X, Vs, Y) :-
	memberchk(X-Y, Vs),
	!.
vs_lookup(X, _, X).

%%%% 
%%%% For qdimacs version 1.1. Fails if no suitable arrangement of
%%%% quantifiers is found due to repeated variables.
%%%%
cleanup_quants([ex([])|Qs], SoFar, Sig, Qs1) :-
	!,
	cleanup_quants(Qs, SoFar, Sig, Qs1).
cleanup_quants([all([])|Qs], SoFar, Sig, Qs1) :-
	!,
	cleanup_quants(Qs, SoFar, Sig, Qs1).
cleanup_quants([Q, ex([])|Qs], SoFar, Sig, Qs1) :-
	!,
	cleanup_quants([Q|Qs], SoFar, Sig, Qs1).
cleanup_quants([Q, all([])|Qs], SoFar, Sig, Qs1) :-
	!,
	cleanup_quants([Q|Qs], SoFar, Sig, Qs1).
cleanup_quants([ex(X), ex(Y)|Qs], SoFar, Sig, Qs1) :-
	!,
	append(X, Y, Z),
	cleanup_quants([ex(Z)|Qs], SoFar, Sig, Qs1).
cleanup_quants([all(X), all(Y)|Qs], SoFar, Sig, Qs1) :-
	!,
	append(X, Y, Z),
	cleanup_quants([all(Z)|Qs], SoFar, Sig, Qs1).
cleanup_quants([all(X)|Qs], SoFar, Sig, [all(X1)|Qs1]) :-
	!,
	sort(X, X2),
	ord_intersect(X2, Sig, X1),
	ord_disjoint(X1, SoFar), %% This might fail
	ord_union(X1, SoFar, SoFar1),
	( Qs = [] ->
	  %% constraint in competition qdimacs:
	  %% innermost must be existential:
	  Qs1 = [ex([])]
	; cleanup_quants(Qs, SoFar1, Sig, Qs1)
	).
cleanup_quants([ex(X)|Qs], SoFar, Sig, [ex(X1)|Qs1]) :-
	!,
	sort(X, X2),
	ord_intersect(X2, Sig, X1),
	ord_disjoint(X1, SoFar), %% This might fail
	ord_union(X1, SoFar, SoFar1),
	cleanup_quants(Qs, SoFar1, Sig, Qs1).
cleanup_quants([], _, _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% DIFF Experimental
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diff_deq_bi([A|As], [(~A1,A),(A1,~A)|L1], [A-A1|TABLE]) :-
	genpred(A1),
	diff_deq_bi(As, L1, TABLE).
diff_deq_bi([], [], []).

diff_deq_pos([A|As], [(~A,A1)|L1], [A-A1|TABLE]) :-
	genpred(A1),
	diff_deq_pos(As, L1, TABLE).
diff_deq_pos([], [], []).

diff_deq_neg([A|As], [(~A1,A)|L1], [A-A1|TABLE]) :-
	genpred(A1),
	diff_deq_neg(As, L1, TABLE).
diff_deq_neg([], [], []).

diff_form_new(SC, SF, ForgSpec, RenameSpec, ExtraForm) :-
	scope_atomsets_pos_neg_bi(SC, SPOS, SNEG, SBI),
	diff_deq_bi(SBI, LB, TB),
	diff_deq_pos(SPOS, LP, TP),
	diff_deq_neg(SNEG, LN, TN),
	append(LP, LN, L0),
	append(LB, L0, L1),
	append(TP, TN, T0),
	append(TB, T0, T1),
	list_to_disjunction(L1, N1),	
	scope_atomset(SF, SIGN),
	scope_atomset(SC, SIGNSCOPE),
	atomset_subtract(SIGN, SIGNSCOPE, SIGVAR),
	map_genpred(SIGVAR, T4),
	append(T1, T4, RenameSpec),
	( setof(A, B^member(B-A, RenameSpec), ForgSpec) ->
	  true
	; ForgSpec = []
	),
	ExtraForm = N1.

%%%% 
%%%% diff(S,T,F) -- S and T must both be atom scopes and be disjoint
%%%% 
diff_st_form_new(SC, SF, T, ForgSpec, RenameSpec, ExtraForm) :-
	scope_atomsets_pos_neg_bi(SC, _SPOS, _SNEG, SBI),
	diff_deq_bi(SBI, L1, T1),
	list_to_disjunction(L1, N1),
	scopespec_scope_intersect(T, SF, SKeep),
	scope_atomset(SKeep, SKeepAtomset),
	scope_atomset(SF, SIGN),
	atomset_subtract(SIGN, SBI, SIGVAR0),
	atomset_subtract(SIGVAR0, SKeepAtomset, SIGVAR),
	map_genpred(SIGVAR, T4),
	append(T1, T4, RenameSpec),
	( setof(A, B^member(B-A, RenameSpec), ForgSpec) ->
	  true
	; ForgSpec = []
	),
	ExtraForm = N1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Circumscription, Raising
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

raise_form_new(SC, SF, ForgSpec, RenameSpec, ExtraForm) :-
	scope_atomsets_pos_neg_bi(SC, SPOS, SNEG, SBI),
	raise_pos_1_new(SPOS, NL1, NL2, T1),
	raise_neg_1_new(SNEG, NL3, NL4, T2),
	append(NL1, NL3, NL5),
	list_to_conjunction(NL5, N5),
	append(NL2, NL4, NL6),
	list_to_disjunction(NL6, N6),
	append(T1, T2, T3),
	scope_atomset(SF, SIGN),
	atomset_subtract(SIGN, SBI, SIGVAR),
	map_genpred(SIGVAR, T4),
	append(T3, T4, RenameSpec),
	( setof(A, B^member(B-A, RenameSpec), ForgSpec) ->
	  true
	; ForgSpec = []
	),
	ExtraForm = (N5, N6).

raise_pos_1_new([A|As], [(~A1;A)|NL1], [(~A1,A)|NL2], [A-A1|TABLE]) :-
	genpred(A1),
	raise_pos_1_new(As, NL1, NL2, TABLE).
raise_pos_1_new([], [], [], []).

raise_neg_1_new([A|As], [(A1;~A)|NL1], [(A1,~A)|NL2], [A-A1|TABLE]) :-
	genpred(A1),
	raise_neg_1_new(As, NL1, NL2, TABLE).
raise_neg_1_new([], [], [], []).


/*
We apply the following to set the circumscription scope:

1. circ(SC, F:SP) ==
       circ(biscope(SC) cup (uniscope(SC) cap SP)) &      
       (bigwedge_L in (uniscope(SC) - SP): ~L)     

   - the uniscope can be restriced to SP + units outside SP
   (see removed_from_pc28.tex)

2. - the biscope can be made smaller under precondition

     Conditions:
     1. SF =< SP cup dus(SP)
     2. SP =< SC
     3. u(SC) = u(SP)

     Objective: find such an SP
     Precondition: SF =< (SC cup dus(SC))   % SF = (SC cup dus(SC)) cap SF
     Let SP = (b(SC) cap (SF cup dus(SF))) cup u(SC)
     Condition 2,3 follow immediately
     Condition 1:     
     SP cup dus(SP) =
     (b(SC) cap (SF cup dus(SF))) cup u(SC) cud dus(u(SC))
     >= (SC cup dus(SC)) cap SF
     = SF

     If SF subseteq (SC cup duals(SC)), then:
     circ(SC, F:SF) ==
        circ( (biscope(SC) cap (SF cup duals(SF))) cup uniscope(SC) )
*/     

circ_params(SC, SP, SC1, Units, UnitsRest) :-	
	( scope_cup_complements(SC, SCA),
	  scope_subseteq(SP, SCA) ->
	  scope_cup_complements(SP, SPA),
	  scope_biscope(SC, SCB),
	  scope_intersection(SCB, SPA, SCB1),
	  scope_uniscope(SC, SCU),
	  scope_union(SCB1, SCU, SC2)
	; SC2 = SC
	),
	scope_uniscope(SC2, SCU2),
	scope_intersection(SCU2, SP, SCU2_IN),
	scope_subtract(SCU2, SP, SCU2_OUT),
	scope_biscope(SC2, SCB2),
	scope_union(SCB2, SCU2_IN, SC1),
	circ_units(SCU2_OUT, Units, UnitsRest).

circ_units([+A|Ls], (~A, Ls1), X) :-
	circ_units(Ls, Ls1, X).
circ_units([-A|Ls], (A, Ls1), X) :-
	circ_units(Ls, Ls1, X).
circ_units([], X, X).

mk_fsopts(RefPs, RefFs, fsopts(RefPs, RefFs)).

fsopts_refsig(fsopts(RefPs, RefFs), RefPs, RefFs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% SRC to NX
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

src_to_nx(F, F1) :-
	src_reference_signature(F, Ps, Fs),
	mk_fsopts(Ps, Fs, Opts),
	src_to_nx(F, Opts, F1).

src_scopetype(F, Opts, T) :-
	src_to_nx(F, Opts, F1),
	nx_scopetype(F1, T).
	
src_to_nx(~(~F), O, F1) :-
	!,
	src_to_nx(F, O, F1).
src_to_nx(~F, O, F1) :-
	!,
	src_to_nx(F, O, F2),
	nx_negate(F2, F1).
src_to_nx((F -> G), O, F1) :-
	!,
	src_to_nx((~F;G), O, F1).
src_to_nx((F <- G), O, F1) :-
	!,
	src_to_nx((F;~G), O, F1).
src_to_nx((F <-> G), O, F1) :-
	!,
	src_to_nx(((F,G);(~F,~G)), O, F1).
src_to_nx(raise(S, F), O, F1) :-
	!,
	fsopts_refsig(O, Ps, Fs),
	scopespec_expand(S, Ps, Fs, SC),
	( scope_is_atom_scope(SC) ->
	  F1 = false
	; src_scopetype(F, O, SP),
	  raise_form_new(SC, SP, ForgSpec, RenameSpec, ExtraForm),
	  F2 = forg(ForgSpec, (rename(RenameSpec, F), ExtraForm)),
	  src_to_nx(F2, O, F1)
	).
src_to_nx(diff(S, T, F), O, F1) :-
	!,
	fsopts_refsig(O, Ps, Fs),
	scopespec_expand(S, Ps, Fs, SC),
	( scope_is_atom_scope(SC) ->
	  src_scopetype(F, O, SP),
	  diff_st_form_new(SC, SP, T, ForgSpec, RenameSpec, ExtraForm),
	  F2 = forg(ForgSpec, (rename(RenameSpec, F), ExtraForm)),
	  src_to_nx(F2, O, F1)
	; err('Diff/3 on literal scope is not supported')
	).
src_to_nx(diff(S, F), O, F1) :-
	!,
	fsopts_refsig(O, Ps, Fs),
	scopespec_expand(S, Ps, Fs, SC),
	src_scopetype(F, O, SP),
	diff_form_new(SC, SP, ForgSpec, RenameSpec, ExtraForm),
	F2 = forg(ForgSpec, (rename(RenameSpec, F), ExtraForm)),
	src_to_nx(F2, O, F1).
src_to_nx(circ(S, F), O, F1) :-
 	!,
	fsopts_refsig(O, Ps, Fs),
	scopespec_expand(S, Ps, Fs, SC0),	
	src_scopetype(F, O, SP),
	circ_params(SC0, SP, SC, Units, UnitsRest),
	F2 = Units,
	UnitsRest = (F, ~raise(SC, F)),
	src_to_nx(F2, O, F1).
src_to_nx(proj(S, F), O, F1) :-
 	!,
 	src_scopetype(F, O, SF),
 	scopespec_scope_intersect(S, SF, SP),
	scope_subtract(SF, SP, KSP),
	F2 = forg(KSP, F),
	src_to_nx(F2, O, F1).
src_to_nx(forg(S, F), O, forg(SP, FS)) :-
 	!,
	src_to_nx(F, O, FS),
	nx_scopetype(FS, SF),
 	scopespec_scope_intersect(S, SF, SP).
src_to_nx(rename(RS, F), O, rename(RS, F1)) :-
	!,
	src_to_nx(F, O, F1).
src_to_nx((F,G), O, (F1, G1)) :-
 	!,
 	src_to_nx(F, O, F1),
 	src_to_nx(G, O, G1).
src_to_nx((F;G), O, (F1; G1)) :-
 	!,
 	src_to_nx(F, O, F1),
 	src_to_nx(G, O, G1).
src_to_nx(true, _, true) :-
	!.
src_to_nx(false, _, false) :-
	!.
src_to_nx(~A, _, -A) :-
	!.
src_to_nx(A, _, +A).

nx_negate(+A, -A) :- !.
nx_negate(-A, +A) :- !.
nx_negate(true, false) :- !.
nx_negate(false, true) :- !.
nx_negate((F,G), (F1;G1)) :- !,	nx_negate(F, F1), nx_negate(G, G1).
nx_negate((F;G), (F1,G1)) :- !,	nx_negate(F, F1), nx_negate(G, G1).
nx_negate(forg(S, F), ~forg(S, F)) :- !.
nx_negate(~forg(S, F), forg(S, F)) :- !.
nx_negate(rename(RS, F), ~rename(RS, F)) :- !.
nx_negate(~rename(RS, F), rename(RS, F)) :- !.

nx_to_src(true, true) :-
	!.
nx_to_src(false, false) :-
	!.
nx_to_src(+F, F) :-
	!.
nx_to_src(-F, ~F) :-
	!.
nx_to_src((F,G), (F1,G1)) :-
	!,
	nx_to_src(F, F1),
	nx_to_src(G, G1).
nx_to_src((F;G), (F1;G1)) :-
	!,
	nx_to_src(F, F1),
	nx_to_src(G, G1).
nx_to_src(forg(S, F), forg(S, F1)) :-
	!,
	nx_to_src(F, F1).
nx_to_src(rename(RS, F), rename(RS, F1)) :-
	!,
	nx_to_src(F, F1).
nx_to_src(~F, ~F1) :-
	!,
	nx_to_src(F, F1).
nx_to_src(F, _) :-
	err('nx_to_src failed: ~w', [F]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%%  Operations on Formulas in SRC Format
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% F is a formula in user syntax. This signature is used as basis
%%%% for circumcription.
%%%% 
src_reference_signature(F, Ps, Fs) :-
	refsig1(F, SS, SF),
	sort(SS, SS1),
	sort(SF, SF1),
	esf_s_matrix(SS1, Matrix),
	m_signature([SF1|Matrix], Ps1, Fs1),
	( \+ memberchk(_/0, Fs1) ->
	  Fs = [dummy_constant/0]
	; Fs = Fs1
	),
	folsig_get_involved_groups(Ps1, Gs),
	folsig_del_specials(Ps1, Ps2),
	folsig_replicate_in_groups(Ps2, Gs, Ps).

folsig_get_involved_groups(Ps, Groups) :-
	%% get_group works also with numbers that are used as
	%% group specifier.
	( setof(N, P^A^(member(P/A, Ps), get_group(P,N)), Groups) ->
	  true
	; Groups = []
	).

folsig_replicate_in_groups(Ps, Ns, Ps1) :-
	frg(Ns, Ps, Ps2),
	sort(Ps2, Ps1).

frg([N|N1], Ps, Ps1) :-
	map_p_set_group(Ps, N, Ps1, Ps2),
	frg(N1, Ps, Ps2).
frg([], _, []).

map_p_set_group([P/A|Xs], N, [P1/A|Xs1], X) :-
	set_group(P, N, P1),
	map_p_set_group(Xs, N, Xs1, X).
map_p_set_group([], _, X, X).

refsig1(F, S1, S2) :-
	logopp_unary(F, F1),
	!,
	refsig1(F1, S1, S2).
refsig1(F, S1, S2) :-
	logopp_binary(F, FF, FG),
	!,
	refsig1(FF, SF1, SF2),
	refsig1(FG, SG1, SG2),
	append(SF1, SG1, S1),
	append(SF2, SG2, S2).
refsig1(F, S1, S2) :-
	logopp_scoped(F, FS, FF),
	!,
	refsig1(FF, S3, S2),
	( FS = complements(FS1) -> true
	; FS1 = FS
	),
	( (FS1 = [] ; FS1 = [_|_]) ->
	  S4 = FS1
	; %% scope specifiers such as pos, neg:
	  S4 = []
	),  
	append(S4, S3, S1).
refsig1(F, S1, S2) :-
	logopp_rename(F, FS, FF),
	!,
	refsig1(FF, S3, S2),
	append(FS, S3, S1).

refsig1(F, [], []) :-
	logopp_constant(F),
	!.
refsig1(F, [], [F]).

%%%% 
%%%% logopp_... This is used for both formulas syntaxes: src and nx
%%%% 
logopp_constant(true).
logopp_constant(false).

logopp_unary(~(F), F).

logopp_binary((F , G), F, G).
logopp_binary((F ; G), F, G).
logopp_binary((F -> G), F, G).
logopp_binary((F <- G), F, G).
logopp_binary((F <-> G), F, G).

logopp_scoped(proj(S, F), S, F).
logopp_scoped(forg(S, F), S, F).
logopp_scoped(circ(S, F), S, F).
logopp_scoped(raise(S, F), S, F).
logopp_scoped(diff(S, F), S, F).

logopp_rename(rename(Spec, F), S, F) :-
	findall(N, (member(N1-N2, Spec), (N = N1 ; N = N2)), S1),
	sort(S1, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Syntax Stuff
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nx_is_plain((F , G)) :-
	!,
	nx_is_plain(F),
	nx_is_plain(G).
nx_is_plain((F ; G)) :-
	!,
	nx_is_plain(F),
	nx_is_plain(G).
nx_is_plain(true) :-
	!.
nx_is_plain(false) :-
	!.
nx_is_plain(+_) :-
	!.
nx_is_plain(-_) :-
	!.

nx_scopetype(F, S) :-
	nx_scopetype_2(F, S1),
	sort(S1, S).

nx_scopetype_2(+A, [+A]) :-
	!.
nx_scopetype_2(-A, [-A]) :-
	!.
nx_scopetype_2((F , G), TFG) :-
	!,
	nx_scopetype_2(F, TF),
	nx_scopetype_2(G, TG),
	append(TF, TG, TFG).
nx_scopetype_2((F ; G), TFG) :-
	!,
	nx_scopetype_2(F, TF),
	nx_scopetype_2(G, TG),
	append(TF, TG, TFG).
nx_scopetype_2(true, []) :-
	!.
nx_scopetype_2(false, []) :-
	!.
nx_scopetype_2(~(F), TFN) :-
	!,
	nx_scopetype_2(F, TF),
	sort(TF, TF1),
	scope_complements(TF1, TFN).
nx_scopetype_2(forg(S, F), TFF) :-
	!,
	nx_scopetype_2(F, TF),
	sort(TF, TF1),
	scope_subtract(TF1, S, TFF).
nx_scopetype_2(rename(RS, F), TFR) :-
	!,
 	nx_scopetype_2(F, TF),
	sort(TF, TF1),
	rename_scope(TF1, RS, TFR).		     
nx_scopetype_2(F, _) :-
	err('Bad invocation of nx_scopetype: ~w', [F]).


nx_scopetype_1(+A, [+A]) :-
	!.
nx_scopetype_1(-A, [-A]) :-
	!.
nx_scopetype_1((F , G), TFG) :-
	!,
	nx_scopetype_1(F, TF),
	nx_scopetype_1(G, TG),
	scope_union(TF, TG, TFG).
nx_scopetype_1((F ; G), TFG) :-
	!,
	nx_scopetype_1(F, TF),
	nx_scopetype_1(G, TG),
	scope_union(TF, TG, TFG).
nx_scopetype_1(true, []) :-
	!.
nx_scopetype_1(false, []) :-
	!.
nx_scopetype_1(~(F), TFN) :-
	!,
	nx_scopetype_1(F, TF),
	scope_complements(TF, TFN).
nx_scopetype_1(forg(S, F), TFF) :-
	!,
	nx_scopetype_1(F, TF),
	scope_subtract(TF, S, TFF).
nx_scopetype_1(rename(RS, F), TFR) :-
	!,
 	nx_scopetype_1(F, TF),
	rename_scope(TF, RS, TFR).		     
nx_scopetype_1(F, _) :-
	err('Bad invocation of nx_scopetype: ~w', [F]).

nx_size(F, N) :-
	logopp_unary(F, F1),
	!,
	nx_size(F1, N1),
	N is 1 + N1.
nx_size(F, N) :-
	logopp_binary(F, F1, F2),
	!,
	nx_size(F1, N1),
	nx_size(F2, N2),
	N is N1 + N2.
nx_size(F, N) :-
	logopp_scoped(F, _, F1),
	!,
	nx_size(F1, N1),
	N is 1 + N1.
nx_size(F, N) :-
	logopp_rename(F, _, F1),
	!,
	nx_size(F1, N1),
	N is 1 + N1.
nx_size(F, 0) :-
	logopp_constant(F),
	!.
nx_size(+_, 1) :-
	!.
nx_size(-_, 1) :-
	!.
nx_size(X, _) :-
	err('nx_size failed: ~q', [X]).

logcomp_nx(forg(S,F), F, forg(S,G), G).
logcomp_nx(proj(S,F), F, proj(S,G), G).
logcomp_nx(rename(S,F), F, rename(S,G), G).
logcomp_nx(circ(S,F), F, circ(S,G), G).
logcomp_nx(raise(S,F), F, raise(S,G), G).
logcomp_nx(~(F), F, ~(G), G).
logcomp_nx((F1,F2), F1, (G1,F2), G1).
logcomp_nx((F1,F2), F2, (F1,G2), G2).
logcomp_nx((F1;F2), F1, (G1;F2), G1).
logcomp_nx((F1;F2), F2, (F1;G2), G2).

subformula_nx(F, F, G, G).
subformula_nx(F, S, G, T) :-
	logcomp_nx(F, F1, G, G1),
	subformula_nx(F1, S, G1, T).

subformula_inner_first_nx(F, S, G, T) :-
	logcomp_nx(F, F1, G, G1),
	subformula_inner_first_nx(F1, S, G1, T).
subformula_inner_first_nx(F, F, G, G).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% NX SIMP LRPO
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rewrite_nx_simp(F, F1) :-
% 	( get_conf(simp_aux_method, 2) ->
% 	  rewrite_nx_rule_cheap(F, F2)
% 	; rewrite_nx_rule_lrpo(F, F2)
% 	),
% 	nx_unit(F2, [], Modified, F3),
% 	( nonvar(Modified) ->
% 	  rewrite_nx_simp(F3, F1)
% 	; F1 = F2
% 	).

rewrite_nx_rule_lrpo(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_rule_lrpo(S, T),
	!,
	rewrite_nx_rule_lrpo(F2, F1).
rewrite_nx_rule_lrpo(F, F).

nx_rule_lrpo((F,(F,H)), (F,H)).
nx_rule_lrpo((F;(F;H)), (F;H)).
nx_rule_lrpo((F,F), F).
nx_rule_lrpo((F;F), F).
nx_rule_lrpo((true,F), F).
nx_rule_lrpo((false, _), false).
nx_rule_lrpo((true;_), true).
nx_rule_lrpo((false; F), F).
nx_rule_lrpo((F,true), F).
nx_rule_lrpo((_,false), false).
nx_rule_lrpo((_;true), true).
nx_rule_lrpo((F;false), F).
nx_rule_lrpo((F,G), (G,F)) :-
	lrpo_greater(F, G).
nx_rule_lrpo((F;G), (G;F)) :-
	lrpo_greater(F, G).
nx_rule_lrpo((F,(G,H)), (G,(F,H))) :-
	lrpo_greater(F, G).
nx_rule_lrpo((F;(G;H)), (G;(F;H))) :-
	lrpo_greater(F, G).


rewrite_nx_rule_cheap(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_rule_cheap(S, T),
	!,
	rewrite_nx_rule_cheap(F2, F1).
rewrite_nx_rule_cheap(F, F).

nx_rule_cheap((F,(F,H)), (F,H)).
nx_rule_cheap((F;(F;H)), (F;H)).
nx_rule_cheap((F,F), F).
nx_rule_cheap((F;F), F).
nx_rule_cheap((true,F), F).
nx_rule_cheap((false, _), false).
nx_rule_cheap((true;_), true).
nx_rule_cheap((false; F), F).
nx_rule_cheap((F,true), F).
nx_rule_cheap((_,false), false).
nx_rule_cheap((_;true), true).
nx_rule_cheap((F;false), F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Some Unit Reduction, to be applied after LRPO-Simplification (such
%%%% that units are left of complex formulas).
%%%% Similar to pullout, but just scans once over the formula. 
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nx_is_literal(+A, +A).
nx_is_literal(-A, -A).

nx_unit(F, US, CH, F1) :-
	nx_is_literal(F, L),
	!,
	( memberchk(L, US) ->
	  F1 = true,
	  CH = modified
	; lit_complement(L, LN),
	  memberchk(LN, US) ->
	  F1 = false,
	  CH = modified
	; F1 = F
	).
nx_unit((F, G), US, CH, F1) :-
	nx_is_literal(F, L),
	!,
	( memberchk(L, US) ->
	  nx_unit(G, US, modified, F1)
	; lit_complement(L, LN),
	  memberchk(LN, US) ->
	  F1 = false,
	  CH = modified
	; F1 = (F, F2),
	  nx_unit(G, [L|US], CH, F2)
	).
nx_unit((F, G), US, CH, (F1, G1)) :-
	!,
	nx_unit(F, US, CH, F1),
	nx_unit(G, US, CH, G1).
nx_unit((F ; G), US, CH, F1) :-
	nx_is_literal(F, L),
	lit_complement(L, LN),
	!,
	( memberchk(LN, US) ->
	  nx_unit(G, US, modified, F1)
	; memberchk(L, US) ->
	  F1 = true,
	  CH = modified
	; F1 = (F ; F2),
	  nx_unit(G, [LN|US], CH, F2)
	).
nx_unit((F ; G), US, CH, (F1 ; G1)) :-
	!,
	nx_unit(F, US, CH, F1),
	nx_unit(G, US, CH, G1).
%% 
%% it seems that we could propagate values inside forg and rename if the
%% forgotten atoms would be given fresh names
%% 
nx_unit(forg(S, F), _, CH, forg(S, F1)) :-
	!,
	nx_unit(F, [], CH, F1).
nx_unit(rename(RS, F), _, CH, rename(RS, F1)) :-
	!,
	nx_unit(F, [], CH, F1).
nx_unit(F, _, _, F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% NX PRE-UNIT
%%%%
%%%% EXPERIMENTAL: Idea: Move units to the left in preparation for UNIT,
%%%% but this should cheaper than rewrite_nx_rule_lrpo
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


rewrite_nx_rule_unitpre(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_rule_unitpre(S, T),
	!,
	rewrite_nx_rule_unitpre(F2, F1).
rewrite_nx_rule_unitpre(F, F).

nx_is_literal(+_).
nx_is_literal(-_).

nx_rule_unitpre((F,(F,H)), (F,H)).
nx_rule_unitpre((F;(F;H)), (F;H)).
nx_rule_unitpre((F,F), F).
nx_rule_unitpre((F;F), F).
nx_rule_unitpre((true,F), F).
nx_rule_unitpre((false, _), false).
nx_rule_unitpre((true;_), true).
nx_rule_unitpre((false; F), F).
nx_rule_unitpre((F,true), F).
nx_rule_unitpre((_,false), false).
nx_rule_unitpre((_;true), true).
nx_rule_unitpre((F;false), F).
nx_rule_unitpre((F,G), (G,F)) :-
	nx_is_literal(G),
	\+ nx_is_literal(F).
nx_rule_unitpre((F;G), (G;F)) :-
	nx_is_literal(G),
	\+ nx_is_literal(F).
nx_rule_unitpre((F,(G,H)), (G,(F,H))) :-
	nx_is_literal(G),
	\+ nx_is_literal(F).
nx_rule_unitpre((F;(G;H)), (G;(F;H))) :-
	nx_is_literal(G),
	\+ nx_is_literal(F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% DECOMPOSE
%%%%
%%%% Experimental: Integration of decomposition in various variants
%%%%
%%%% Decomposition into Connected Components: Conjuncts that are connected via
%%%% a link in the supplied scope. That is, a literal in the scope that is in
%%%% some conjunct is complementary in another conjunct.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Thu Apr 18 21:45:37 2013
%%%% might be bugged
%%%% 
dec_binary_inward(S, F, G, FG1) :-
	nx_scopetype(F, SF),
	nx_scopetype(G, SG),
	scope_atomset(S, As),
	map_dbi(As, S, SF, SG, S1, SF1, SG1),
	sort(S1, S2),
	S2 \= S,
	sort(SF1, SF2),
	sort(SG1, SG2),
	( SF2 = [] -> F1 = F ; F1 = forg(SF2, F) ),
	( SG2 = [] -> G1 = G ; G1 = forg(SG2, G) ),
	( S2 = [] -> FG1 = (F1, G1) ; FG1 = forg(S2, (F1, G1)) ).

map_dbi([A|As], S, SF, SG, S1, SF1, SG1) :-
	PA = +A,
	NA = -A,
	( scope_memberchk(PA, SF) -> PF = y ; PF = n ),
	( scope_memberchk(NA, SF) -> NF = y ; NF = n ),
	( scope_memberchk(PA, SG) -> PG = y ; PG = n ),
	( scope_memberchk(NA, SG) -> NG = y ; NG = n ),
	( scope_memberchk(PA, S) -> PS = y ; PS = n ),
	( scope_memberchk(NA, S) -> NS = y ; NS = n ),
	%% pn/p/n in S
	%% pn/p/n in F x pn/p/n in G
	(((PF = y, NG = y) ; (NF = y, PG = y)) ->
	  %% in a link
	  (PS = y, NS = y ->
	    ((PF = y ; PG = y) ->
	      ((NF = y ; NG = y) ->
		S1 = [PA,NA|S2]
	      ; S1 = [PA|S2]
	      )
	    ; S1 = [NA|S2]
	    )
	  ; PS = y ->
	    S1 = [PA|S2]
	  ; S1 = [NA|S2]
	  ),
	  SF1 = SF2,
	  SG1 = SG2
	; S1 = S2,
	  ( PS = y ->
	    ( PF = y -> SF1 = [PA|SF3] ; SF1 = SF3 ),
	    ( PG = y -> SG1 = [PA|SG3] ; SG1 = SG3 )
	  ; SF1 = SF3,
	    SG1 = SG3
	  ),
	  ( NS = y ->
	    ( NF = y -> SF3 = [NA|SF2] ; SF3 = SF2 ),
	    ( NG = y -> SG3 = [NA|SG2] ; SG3 = SG2 )
	  ; SF3 = SF2,
	    SG3 = SG2
	   
	  )
	),
	map_dbi(As, S, SF, SG, S2, SF2, SG2).
map_dbi([], _, _, _, [], [], []).
	  
	   

         
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simp_nx_loop_1(F, F1) :-
%    	simp_nx_unit_1(F, F2),
% 	get_conf(decomp_method, M),
% 	( M = 0 ->
% 	  rewrite_nx_main(F2, F1)
% 	; M = 1 ->
% 	  rewrite_nx_main_decomp_binary(F2, F1)
% 	; M = 3 ->
% 	  rewrite_nx_main_decomp_scope(F2, F1)
% 	; rewrite_nx_main_decomp_step(F2, F1)
% 	).

rewrite_nx_decomp(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_decomp(S, T),
	!,
	rewrite_nx_decomp(F2, F1).
rewrite_nx_decomp(F, F).


rewrite_nx_main_decomp_step(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_main(S, T),
	!,
	rewrite_nx_main_decomp_step(F2, F1).
rewrite_nx_main_decomp_step(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_decomp(S, T),
	!,
	rewrite_nx_main_decomp_step(F2, F1).
rewrite_nx_main_decomp_step(F, F).

rewrite_nx_main_decomp_scope(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_main(S, T),
	!,
	rewrite_nx_main_decomp_scope(F2, F1).
rewrite_nx_main_decomp_scope(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_decomp_scope(S, T),
	!,
	rewrite_nx_main_decomp_scope(F2, F1).
rewrite_nx_main_decomp_scope(F, F).

rewrite_nx_main_decomp_binary(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_main(S, T),
	!,
	rewrite_nx_main_decomp_binary(F2, F1).
rewrite_nx_main_decomp_binary(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_decomp_binary(S, T),
	!,
	rewrite_nx_main_decomp_binary(F2, F1).
rewrite_nx_main_decomp_binary(F, F).

nx_decomp(forg(S, F), F1) :-
	nx_dec_step(S, F, F1).

nx_decomp_scope(forg(S, F), F1) :-
	nx_dec_scope(S, F, F1).

nx_decomp_binary(forg(S, (F , G)), (forg(S, F) , forg(S, G))) :-
   	nx_scopetype(F, SF),
   	nx_scopetype(G, SG),
   	linkless_inside(SF, SG, S).

% nx_decomp(forg(S, F), forg(S1, F)) :-
%   	nx_scopetype(F, SF),
%  	scope_intersection(S, SF, S1),
%  	S1 \= S.

%% seems slow
%%
% rewrite_nx_main_decomp(F, F1) :-
% 	subformula_nx(F, S, F2, T),
% 	( nx_main(S, T)
% 	; nx_decomp(S, T)
% 	),
% 	!,
% 	rewrite_nx_main_decomp(F2, F1).
% rewrite_nx_main_decomp(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nx_dec_step(S, F, F1) :-
	conjunction_to_list(F, Fs),
	Fs = [_, _|_],
	select(L, S, S0),
	%% ? todo ... select just once if same atom
	lit_complement(L, L1),
	classify_by_l_in_scopetype(Fs, L, L1, None, Pos, Neg, Both),
	( select(L1, S0, S1) -> sort([L,L1], SL) ; S1 = S0, SL = [L] ),
	( Both = [] ->
	  ( Pos = [] ->
	    ( Neg = [] ->
	      list_to_conjunction(None, FNone),
	      F2 = forg(S1, FNone)
	    ; dist_forg(Neg, SL, FNeg),
	      ( None = [] ->
		F2 = forg(S1, FNeg)
	      ; list_to_conjunction(None, FNone),
		F2 = forg(S1, (FNeg, FNone))
	      )
	    )
	  ; Neg = [] ->
	    dist_forg(Pos, SL, FPos),
	    ( None = [] ->
	      F2 = forg(S1, FPos)
	    ; list_to_conjunction(None, FNone),
	      F2 = forg(S1, (FPos, FNone))
	    )
	  ; None = [] ->
	    fail
	  ; list_to_conjunction(None, FNone),
	    append(Pos, Neg, PN),
	    list_to_conjunction(PN, FPN),
	    F2 = forg(S1, (forg(SL, FPN), FNone))
	  )
	; None = [] ->
	  fail
	; list_to_conjunction(None, FNone),
	  append(Pos, Neg, PN1),
	  append(Both, PN1, PN),
	  list_to_conjunction(PN, FPN),
	  F2 = forg(S1, (forg(SL, FPN), FNone))
	),
	( F2 = forg([], X1) ->
	  F1 = X1
	; F2 = F1
	).


classify_by_l_in_scopetype(Fs, L, L1, None, Pos, Neg, Both) :-
	cbls(Fs, L, L1, None, Pos, Neg, Both).

classify_by_a_in_scopetype(Fs, A, None, Some) :-
	cbls(Fs, +A, -A, None, Pos, Neg, Both),
	append(Pos, Neg, Single),
	append(Single, Both, Some).

cbls([F|Fs], L, L1, None, Pos, Neg, Both) :-
	nx_scopetype(F, S),
	( scope_memberchk(L, S) ->
	  ( scope_memberchk(L1, S) ->
	    None = None1,
	    Pos = Pos1,
	    Neg = Neg1,
	    Both = [F|Both1]	    
	  ; None = None1,
	    Pos = [F|Pos1],
	    Neg = Neg1,
	    Both = Both1
	  )
	; scope_memberchk(L1, S) ->
	  None = None1,
	  Pos = Pos1,
	  Neg = [F|Neg1],
	  Both = Both1
	; None = [F|None1],
	  Pos = Pos1,
	  Neg = Neg1,
	  Both = Both1
	),
	cbls(Fs, L, L1, None1, Pos1, Neg1, Both1).
cbls([], _, _, [], [], [], []).

dist_forg([F], S, forg(S, F)) :-
	!.
dist_forg([F|Fs], S, (forg(S, F), Fs1)) :-
	dist_forg(Fs, S, Fs1).
dist_forg([], _, true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% tst0 :-
% 
% 	X = ((-q;+p), (+q;-q)),
% 	Y = (-q,+p;-r,+p1),
% 	S = [+q],
% 	nx_decompose_forg_over_scope((X,Y), S, _Fs).


%%%% 
%%%% Fails if no proper decomposition is possible
%%%%
%%%% Sometimes SWI errors related to the destructive hashtables used here
%%%%
nx_dec_scope(S, F, Fs) :-
	conjunction_to_list(F, Fs1),
	Fs1 = [_, _ | _],
	nx_decompose_formulas(Fs1, S, Components),
	Components \= [_],
%	findall(KK, (member(K1, Components), member(KK, K1)), FF0),
% 	sort(FF0, FF1),
% 	sort(Fs1, FF2),
% 	( FF1 \= FF2 ->
% 	  true
% 	; true
% 	),
	( member(X0, Components), member(Y0, Components), X0 \= Y0,
	  list_to_conjunction(X0, XX0),
	  list_to_conjunction(Y0, YY0),
	  nx_scopetype(XX0, XXX1),
	  nx_scopetype(YY0, YYY1),
	  \+ linkless_inside(XXX1, YYY1, S) ->
	  nl,
	  writeln('not-linkless'),
	  writeq(XX0), nl,
	  writeq(YY0), nl,
	  writeln(s0(XXX1)),
	  writeln(s1(YYY1)),
	  writeq(scope(S)),
	  nl
	; true
	),
	
	map_forg(Components, S, Fs).

map_forg([X], S, forg(S,X1)) :-
	!,
	list_to_conjunction(X, X1).
map_forg([X|Xs], S, (forg(S,X1), Xs1)) :-
	list_to_conjunction(X, X1),
	map_forg(Xs, S, Xs1).
map_forg([], _, true).

nx_decompose_formulas(Fs, S, Components) :-
	scope_cup_complements(S, S1),
	nx_formulas_to_g(Fs, S1, G),
	ccs(G, Components).

nx_formulas_to_g(Fs, S, g(Fs1, HTG)) :-
	sort(Fs, Fs1),
	length(Fs1, Size1),
	mk_ht(Size1, FtoLs),
	Size2 is 10 * Size1,
	mk_htm(Size2, LtoFs),
	( member(F, Fs1),
	  nx_scopetype(F, S1),
	  scope_intersection(S1, S, S2),
	  ht_put(FtoLs, F, S2),
	  ( member(L, S2),
	    htm_add(LtoFs, L, F),
	    fail
	  ; true
	  ),
	  fail
	; true
	),
	Size4 is 2 * Size1,
	mk_htm(Size4, HTG),
	( member(F1, Fs1),
	  ht_get(FtoLs, F1, Ls),
	  member(L1, Ls),
	  lit_complement(L1, L2),
	  htm_get(LtoFs, L2, F2),
	  ( htm_get(HTG, F1, F2) ->
	    true
	  ; htm_add(HTG, F1, F2)
	  ),
	  fail
        ; true
	).

graph_nodes(g(Fs,_), Fs).

node_successors(N, g(_,HT), Nodes) :-
	( setof(N1, htm_get(HT, N, N1), Nodes) -> true
	; Nodes = []
	).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%% 
% %%%% Acessors for graphs in S-representation
% %%%% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% graph_nodes(G, Nodes) :-
% 	map_key(G, Nodes).
% 
% map_key([K-_|KVs], [K|Ks]) :-
% 	map_key(KVs, Ks).
% map_key([], []).
% 
% node_successors(N, G, Ns) :-
% 	memberchk(N-Ns, G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Connected components as list of lists of nodes
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
%% Graph accessors graph_nodes/2 and node_successors/3 are assumed to return
%% sorted lists
%% 

ccs(Graph, NodeSets) :-
	graph_nodes(Graph, Nodes),
	ccs(Nodes, Graph, NodeSets).

ccs([N|Unvisited], Graph, [NodeSet|NodeSets]) :-
	node_components(N, Unvisited, Graph, Unvisited1, NodeSet),
	ccs(Unvisited1, Graph, NodeSets).
ccs([], _, []).

node_components(N, U, G, U1, Ns) :-
	node_components(N, U, G, U1, Ns, []).
	
node_components(N, U, G, U1, [N|Ns], NsX) :-
	node_successors(N, G, Ns1),
	ord_intersection(U, Ns1, Ns2),
	ord_subtract(U, Ns1, U2),
	map_nc(Ns2, U2, G, U1, Ns, NsX).

map_nc([N|Ns], U, G, U1, Ns1, NsX) :-
	node_components(N, U, G, U2, Ns1, NsY),
	map_nc(Ns, U2, G, U1, NsY, NsX).
map_nc([], U, _, U, NsX, NsX).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Experimental: Formula Simplifications
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rr_fact(Ls, F, X) :-
	reset_genpred,
	expand_elim_macros(F, F1),
	install_lrpo_for_formula(F1),
	src_to_nx(F1, F2),
	nx_rr_factorize_lits(Ls, F2, F3),
	nx_to_src(F3, X),
	( is_valid_1((F <-> X)) -> writeln(well) ; writeln(bad) ).

is_valid_1(F) :-
	elim(proj([], ~F), X),
	X = false.

rr_comp(A, B, X) :-
	reset_genpred,
	install_lrpo_for_formula((A,B)),
	src_to_nx(A, A1),
	src_to_nx(B, B1),
	( lrpo_greater(A1, B1) ->
	  X = (>)
	; lrpo_greater(B1, A1) ->
	  X = (<)
	; X = (-)
	).

rr0(F, X) :-
	rr0('$dummy', F, X).

rr0(Heavy, F, X) :-
	rr0(Heavy, '$heavy', F, X).

rr1(Light, F, X) :-
	rr0(Light, '$light', F, X).

rr0(Focus, Value, F, X) :-
	reset_genpred,
	expand_elim_macros(F, F1),
	install_lrpo_for_formula(F1),
	src_to_nx(F1, F2),
%	writeln(F2),
	replace_atoms(F2, [Focus-Value], F3),
%	writeln(rrrr(Focus,Value,F3)),
	rewrite_nx_lrpo_basic(F3, F4),
	replace_atoms(F4, [Value-Focus], F5),
	nx_to_src(F5, X).

nx_rr_factorize_atoms(As, F, F1) :-
	%% Standard rewriting first to prevent repetitions in conjunctions and
	%% disjunctions of the atom/literal that later becomes heavy
	rewrite_nx_lrpo_basic(F, F2),
	nx_rr_factorize_atoms_1(As, F2, F1).

nx_rr_factorize_atoms_1([A|As], F, F1) :-
	Focus = '$heavy',
	replace_atom(F, A, Focus, F2),
	rewrite_nx_lrpo_factorize(F2, F3),
	replace_atom(F3, Focus, A, F4),
	nx_rr_factorize_atoms_1(As, F4, F1).
nx_rr_factorize_atoms_1([], F, F).


%%%% 
%%%% The heavy_up_dis rules can serve to factorize bodies in normal logic
%%%% programs: factorize with the uniscope of circumscriptions
%%%% 
%%%% Setting both polarities to heavy might prevent factoring steps:
%%%% ..heavy.. ..~heavy.. ..heavy..
%%%%
nx_rr_factorize_lits(Ls, F, F1) :-
	%% Standard rewriting first to prevent repetitions in conjunctions and
	%% disjunctions of the atom/literal that later becomes heavy
	rewrite_nx_lrpo_basic(F, F2),
	nx_rr_factorize_lits_1(Ls, F2, F3),
%	F1 = F3.
	%% Re-order stuff in the standard way and perform unit simplifications
 	rewrite_nx_main(F3, F1).


nx_rr_factorize_lits_1([L|Ls], F, F1) :-
	FocusAtom = '$heavy',
	( L = +A ->
	  Focus = +FocusAtom
	; L = -A ->
	  Focus = -FocusAtom
	),
	replace_literal(F, L, Focus, F2),
	rewrite_nx_lrpo_factorize(F2, F3),
	replace_atom(F3, FocusAtom, A, F4),
	nx_rr_factorize_lits_1(Ls, F4, F1).
nx_rr_factorize_lits_1([], F, F).

nx_rule_lrpo_factorize(X, Y) :- nx_rule_lrpo_basic(X, Y).
nx_rule_lrpo_factorize(X, Y) :- nx_rule_lrpo_heavy_up_dis(X, Y).
nx_rule_lrpo_factorize(X, Y) :- nx_rule_lrpo_heavy_up_con(X, Y).

rewrite_nx_lrpo_factorize(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_rule_lrpo_factorize(S, T),
	!,
% 	nl, write('                            '), writeln((S -> T)),
%	writeq(F2), nl,
	rewrite_nx_lrpo_factorize(F2, F1).
rewrite_nx_lrpo_factorize(F, F).

rewrite_nx_lrpo_basic(F, F1) :-
	subformula_nx(F, S, F2, T),
	nx_rule_lrpo_basic(S, T),
	!,
	rewrite_nx_lrpo_basic(F2, F1).
rewrite_nx_lrpo_basic(F, F).

nx_rule_lrpo_basic((F,(F,H)), (F,H)).
nx_rule_lrpo_basic((F;(F;H)), (F;H)).
nx_rule_lrpo_basic((F,F), F).
nx_rule_lrpo_basic((F;F), F).
nx_rule_lrpo_basic((true,F), F).
nx_rule_lrpo_basic((false, _), false).
nx_rule_lrpo_basic((true;_), true).
nx_rule_lrpo_basic((false; F), F).
nx_rule_lrpo_basic((F,true), F).
nx_rule_lrpo_basic((_,false), false).
nx_rule_lrpo_basic((_;true), true).
nx_rule_lrpo_basic((F;false), F).
nx_rule_lrpo_basic((F,G), (G,F)) :-
	lrpo_greater(F, G).
nx_rule_lrpo_basic((F;G), (G;F)) :-
	lrpo_greater(F, G).
nx_rule_lrpo_basic((F,(G,H)), (G,(F,H))) :-
	lrpo_greater(F, G).
nx_rule_lrpo_basic((F;(G;H)), (G;(F;H))) :-
	lrpo_greater(F, G).


nx_rule_lrpo_heavy_up_dis((+'$heavy', -'$heavy'), false).
nx_rule_lrpo_heavy_up_dis((+'$heavy', -'$heavy', _), false).
nx_rule_lrpo_heavy_up_dis((Heavy, G), X) :-
	( Heavy = +'$heavy' ; Heavy = -'$heavy' ),
	select_conjunct(G, G1, K),
	mem_heavy_dis(G1, Heavy, _),
	( K = true ->
	  X = Heavy
	; X = (Heavy , K)
	).
nx_rule_lrpo_heavy_up_dis((F,G), X) :-
	( Heavy = +'$heavy' ; Heavy = -'$heavy' ),
	mem_heavy_dis(F, Heavy, F1),
	select_conjunct(G, G2, K),
	mem_heavy_dis(G2, Heavy, G1),
	H = ((F1,G1);Heavy),
	( K = true ->
	  X = H
	; X = (H , K)
	).


nx_rule_lrpo_heavy_up_con((+'$heavy'; -'$heavy'), true).
nx_rule_lrpo_heavy_up_con((+'$heavy'; -'$heavy'; _), true).
nx_rule_lrpo_heavy_up_con((Heavy; G), X) :-
	( Heavy = +'$heavy' ; Heavy = -'$heavy' ),
	select_disjunct(G, G1, K),
	mem_heavy_con(G1, Heavy, _),
	( K = false ->
	  X = Heavy
	; X = (Heavy ; K)
	).
nx_rule_lrpo_heavy_up_con((F;G), X) :-
	( Heavy = +'$heavy' ; Heavy = -'$heavy' ),
	mem_heavy_con(F, Heavy, F1),
	select_disjunct(G, G2, K),
	mem_heavy_con(G2, Heavy, G1),
	H = ((F1;G1),Heavy),
	( K = false ->
	  X = H
	; X = (H ; K)
	).

select_disjunct((F;G), F, G).
select_disjunct((F;G;H), G1, (F;G2)) :-
	!,
	select_disjunct((G;H), G1, G2).
select_disjunct((F;G), G, F) :-
	!.
select_disjunct(F, F, false).

select_conjunct((F,G), F, G).
select_conjunct((F,G,H), G1, (F,G2)) :-
	!,
	select_conjunct((G,H), G1, G2).
select_conjunct((F,G), G, F) :-
	!.
select_conjunct(F, F, true).

mem_heavy_con((G,Heavy), Heavy, G) :-
	!.
mem_heavy_con((F,G), Heavy, (F,G1)) :-
	mem_heavy_con(G, Heavy, G1).

mem_heavy_dis((G;Heavy), Heavy, G) :-
	!.
mem_heavy_dis((F;G), Heavy, (F;G1)) :-
	mem_heavy_dis(G, Heavy, G1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foo((+a,+b,-'$heavy';
     (+a,-r;+b,-r; (+k,-q;+f,+g,-q,+z);-q,+u,+v,+z;+e,-'$heavy');
     (+h;+f,+g),-'$heavy')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% New general simplification for the nx format
%%%% Sat Sep 21 09:27:45 2013
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%% 
%%%% Single Pass Version
%%%% 
rw_main_v1(F, F1) :-
	rw_main_inwards(F, F2),
 	rw_main_outwards(F2, F3),
  	rw_pullout(F3, Modified, F4),
 	( nonvar(Modified) ->
 	  rw_main_outwards(F4, F1)
 	; F1 = F4
 	).

%%%% 
%%%% Looping Version, guarantees no ~/1 in front of ;/2, ,/2
%%%%
rw_main_v1_until_fix(F, F1) :-
	rw_main_inwards(F, F2),
 	rw_main_outwards(F2, F3),
  	rw_pullout(F3, Modified, F4),
 	( nonvar(Modified) ->
 	  rw_main_outwards(F4, F5)
 	; F5 = F4
 	),
	rw_main_v1_until_fix_1(F5, F1).

rw_main_v1_until_fix_1(F, F1) :-
	rw_main_inwards(F, F2),
	( F \= F2 ->
	  info(90, 'rw_main_v1_until_fix: new round'),
	  rw_main_outwards(F2, F3),
	  rw_pullout(F3, Modified, F4),
	  ( nonvar(Modified) ->
	    rw_main_outwards(F4, F5),
	    rw_main_v1_until_fix_1(F5, F1)
	  ; F1 = F4
	  )
	; F1 = F
	).


%%%% 
%%%% 
%%%% 

rw_pullout(F, Modified, F1) :-
	nx_unit(F, [], Modified, F1).

%%%% 
%%%%
%%%% 

unary_logcomp_nx_det(~(F), F, ~G, G).
unary_logcomp_nx_det(forg(S,F), F, forg(S,G), G).
unary_logcomp_nx_det(proj(S,F), F, proj(S,G), G).
unary_logcomp_nx_det(rename(S,F), F, rename(S,G), G).
unary_logcomp_nx_det(circ(S,F), F, circ(S,G), G).
unary_logcomp_nx_det(raise(S, F), F, raise(S,G), G).

nx_compare_greater(+A, +B) :-
	!,
	A @> B.
nx_compare_greater(-A, -B) :-
	!,
	A @> B.
nx_compare_greater(_, +_) :-
	!.
nx_compare_greater(+_, -_) :-
	!,
	fail.
nx_compare_greater(_, -_) :-
	!,
	true.
nx_compare_greater(A, B) :-
	A @> B.

%%%% 
%%%%
%%%% 
rw_main_outwards((F,G), F1) :-
	!,
	rw_main_outwards(F, F2),
	rw_main_outwards(G, G2),
	nx_main_step_conj(F2, G2, F1).
rw_main_outwards((F;G), F1) :-
	!,
	rw_main_outwards(F, F2),
	rw_main_outwards(G, G2),
	nx_main_step_disj(F2, G2, F1).
rw_main_outwards(F, F1) :-
	unary_logcomp_nx_det(F, A, F2, A2),
	!,
	rw_main_outwards(A, A2),
	nx_main_step(F2, F1).
rw_main_outwards(F, F).

nx_insert_conjunct(F, (F, H), F1) :-
	!,
	nx_insert_conjunct(F, H, F1).
nx_insert_conjunct(F, (G, H), (G, F1)) :-
	nx_compare_greater(F, G),
	!,
	nx_insert_conjunct(F, H, F1).
nx_insert_conjunct(F, G, (G, F)) :-
	nx_compare_greater(F, G),
	!.
nx_insert_conjunct(F, G, (F, G)).

nx_insert_disjunct(F, (F; H), F1) :-
	!,
	nx_insert_disjunct(F, H, F1).
nx_insert_disjunct(F, (G; H), (G; F1)) :-
	nx_compare_greater(F, G),
	!,
	nx_insert_disjunct(F, H, F1).
nx_insert_disjunct(F, G, (G; F)) :-
	nx_compare_greater(F, G),
	!.
nx_insert_disjunct(F, G, (F; G)).

nx_main_step_conj(false, _, false) :- !.
nx_main_step_conj(_, false, false) :- !.
nx_main_step_conj(true, F, F) :- !.
nx_main_step_conj(F, true, F) :- !.
nx_main_step_conj(F, F, F) :- !.
nx_main_step_conj(F, G, F1) :-
	nx_insert_conjunct(F, G, F1).


nx_main_step_disj(true, _, true) :- !.
nx_main_step_disj(_, true, true) :- !.
nx_main_step_disj(false, F, F) :- !.
nx_main_step_disj(F, false, F) :- !.
nx_main_step_disj(F, F, F) :- !.
nx_main_step_disj(F, G, F1) :-
	nx_insert_disjunct(F, G, F1).

nx_main_step(~(true), false) :- !.
nx_main_step(~(false), true) :- !.
nx_main_step(~(+A), -A) :- !.
nx_main_step(~(-A), +A) :- !.
nx_main_step(~(~(F)), F) :- !.
nx_main_step(rename(_, true), true) :- !.
nx_main_step(rename(_, false), false) :- !.
nx_main_step(rename(RS, +A), +B) :-
	!,
	rename_atom(RS, A, B).
nx_main_step(rename(RS, -A), -B) :-
	!,
	rename_atom(RS, A, B).
nx_main_step(forg([], F), F) :-
	!.
nx_main_step(forg(_, true), true) :-
	!.
nx_main_step(forg(_, false), false) :-
	!.
nx_main_step(forg(S1, forg(S2, F)), forg(S12, F)) :-
	!,
 	scope_union(S1, S2, S12).
nx_main_step(forg(S, F), F) :-
 	nx_scopetype(F, SF),
 	scope_disjoint(S, SF),
	!.
nx_main_step(forg(S, +A), +A) :-
	\+ scope_memberchk(+A, S),
	!.
nx_main_step(forg(S, -A), -A) :-
	\+ scope_memberchk(-A, S),
	!.
nx_main_step(forg(S, +A), true) :-
	scope_memberchk(+A, S),
	!.
nx_main_step(forg(S, -A), true) :-
	scope_memberchk(-A, S),
	!.
nx_main_step(F, F).
	

%%%% 	
%%%% 	
%%%%
rw_main_inwards((F,G), (F1,G1)) :-
	!,
	rw_main_inwards(F, F1),
	rw_main_inwards(G, G1).
rw_main_inwards((F;G), (F1;G1)) :-
	!,
	rw_main_inwards(F, F1),
	rw_main_inwards(G, G1).
rw_main_inwards(F, F1) :-
	unary_logcomp_nx_det(F, A, F2, A2),
	!,
	rw_main_inwards(A, A2),
	( nx_main_inwards(F2, F3) ->
	  rw_main_inwards(F3, F1)
	; F1 = F2
	).
rw_main_inwards(F, F).

nx_main_inwards(~((F, G)), (~F; ~G)).
nx_main_inwards(~((F; G)), (~F, ~G)).
nx_main_inwards(rename(RS, (F , G)), (rename(RS, F) , rename(RS, G))).
nx_main_inwards(rename(RS, (F ; G)), (rename(RS, F) ; rename(RS, G))).
nx_main_inwards(rename(RS, ~(F)), ~rename(RS, F)).
nx_main_inwards(rename(RS1, rename(RS2, F)), rename(RS, F)) :-
	append(RS2, RS1, RS).
nx_main_inwards(forg(S, (F ; G)), (forg(S, F) ; forg(S, G))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


convert_elim_to_varelim(Vs, [], F, Src, Vs, F, Src) :-
	!.
convert_elim_to_varelim(Vs, Ls, F, _, Vs1, F1, Src1) :-
	!,
	length(Ls, ApproxSize),
	mk_ht(ApproxSize, M),
	map_ev_implication(Ls, M, Is, M1),
	map_key(Is, Vs2),
	append(Vs, Vs2, Vs1),
	map_val(Is, Is1),
	list_to_conjunction(Is1, Is2),
	apply_renaming_table_to_plain_nx(F, M1, F2),
	F1 = (F2, Is2),
	findall(L, (member(A, Vs1), (L = +(A); L=(-A))), S1),
	sort(S1, S2),
	Src1 = forg(S2, F1).

ev_implication(+X, M, Y-(-X;+Y), M1) :-
	!,
	fresh_atom(X, M, Y, M1).
ev_implication(-X, M, Y-(+X;-Y), M1) :-
	!,
	fresh_atom(X, M, Y, M1).

fresh_atom(A, M, A1, M) :-
	ht_get(M, A, A1),
	!.
fresh_atom(A, M, A1, M) :-
	genpred(A1),
	ht_put(M, A, A1).


map_ev_implication([X|Xs], M, [X1|Xs1], M1) :-
	ev_implication(X, M, X1, M2),
	map_ev_implication(Xs, M2, Xs1, M1).
map_ev_implication([], M, [], M).

apply_renaming_table_to_plain_nx((F;G), M, (F1;G1)) :-
	!,
	apply_renaming_table_to_plain_nx(F, M, F1),
	apply_renaming_table_to_plain_nx(G, M, G1).
apply_renaming_table_to_plain_nx((F,G), M, (F1,G1)) :-
	!,
	apply_renaming_table_to_plain_nx(F, M, F1),
	apply_renaming_table_to_plain_nx(G, M, G1).
apply_renaming_table_to_plain_nx(+A, M, +A1) :-
	!,
	( ht_get(M, A, A1) ->
	  true
	; A1 = A
	).
apply_renaming_table_to_plain_nx(-A, M, -A1) :-
	!,
	( ht_get(M, A, A1) ->
	  true
	; A1 = A
	).



