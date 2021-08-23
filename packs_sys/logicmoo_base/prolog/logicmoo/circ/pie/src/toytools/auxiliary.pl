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

:- module(auxiliary, [subexpression/4,
		      subexpression_inner_first/4,
		      toplevel_conjunction_to_list/2,
		      toplevel_disjunction_to_list/2,		      
		      conjunction_to_list/2,
		      disjunction_to_list/2,		      
		      list_to_conjunction/2,
		      list_to_disjunction/2,
		      propositional_cnf_form/2,
		      propositional_cnf_to_form/2,
		      propositional_dnf_form/2,
		      propositional_dnf_to_form/2,
		      dnf_prop/2,
		      cnf_prop/2,
		      cnf_prop_nosimp/2,
		      cnf_prop_fewsimp/2,
		      dnf_prop_nosimp/2,
		      red_subs/2,
		      red_unit/2,
		      red_subsres/2,
		      red_subsres_fwdsubs/2,
		      m_prime/2,
		      mlit_complem/2,
		      mlit_atom/2,
		      m_size/2,
		      m_prop_signature/2,
		      with_time_limit/2]).

:- use_module(nf(nf)).

:- set_prolog_flag(optimise,true).

:- module_transparent with_time_limit/2.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Auxiliary: Subexpression
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subexpression(F, F, G, G).
subexpression(F, S, G, T) :-
	nonvar(F),
	functor(F, Op, N),
	functor(G, Op, N),
	F =.. [Op|Fs],
	G =.. [Op|Gs],
	map_subexpression(Fs, S, Gs, T).

map_subexpression([F|Fs], S, [G|Fs], T) :-
	subexpression(F, S, G, T).
map_subexpression([F|Fs], S, [F|Gs], T) :-
	map_subexpression(Fs, S, Gs, T).

subexpression_inner_first(F, S, G, T) :-
	nonvar(F),
	functor(F, Op, N),
	functor(G, Op, N),
	F =.. [Op|Fs],
	G =.. [Op|Gs],
	map_subexpression_inner_first(Fs, S, Gs, T).
subexpression_inner_first(F, F, G, G).

map_subexpression_inner_first([F|Fs], S, [G|Fs], T) :-
	subexpression_inner_first(F, S, G, T).
map_subexpression_inner_first([F|Fs], S, [F|Gs], T) :-
	map_subexpression_inner_first(Fs, S, Gs, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Auxiliary: Conversion - List / Conjunction, Disjunction
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

toplevel_conjunction_to_list((X,Y), [X|Ys]) :-
	!,
	toplevel_conjunction_to_list(Y, Ys).
toplevel_conjunction_to_list(X, [X]).

toplevel_disjunction_to_list((X;Y), [X|Ys]) :-
	!,
	toplevel_disjunction_to_list(Y, Ys).
toplevel_disjunction_to_list(X, [X]).

conjunction_to_list((F, G), FG) :-
	!,
	conjunction_to_list(F, FL),
	conjunction_to_list(G, GL),
	append(FL, GL, FG).
conjunction_to_list(true, []) :-
	!.
conjunction_to_list(F, [F]).

disjunction_to_list((F; G), FG) :-
	!,
	disjunction_to_list(F, FL),
	disjunction_to_list(G, GL),
	append(FL, GL, FG).
disjunction_to_list(false, []) :-
	!.
disjunction_to_list(F, [F]).

list_to_conjunction([F], F) :-
	!.
list_to_conjunction([F|G], (F, G1)) :-
	list_to_conjunction(G, G1).
list_to_conjunction([], true).

list_to_disjunction([F], F) :-
	!.
list_to_disjunction([F|G], (F; G1)) :-
	list_to_disjunction(G, G1).
list_to_disjunction([], false).

propositional_cnf_form(F, F1) :-
	cnf(F, F2),
	propositional_cnf_to_form(F2, F1).

propositional_cnf_to_form(M, F) :-
	map_list_to_disjunction(M, M1),
	list_to_conjunction(M1, F).

propositional_dnf_form(F, F1) :-
	dnf(F, F2),
	propositional_dnf_to_form(F2, F1).

propositional_dnf_to_form(M, F) :-
	map_list_to_conjunction(M, M1),
	list_to_disjunction(M1, F).

map_list_to_disjunction([X|Xs], [X1|Xs1]) :-
	list_to_disjunction(X, X1),
	map_list_to_disjunction(Xs, Xs1).
map_list_to_disjunction([], []).

map_list_to_conjunction([X|Xs], [X1|Xs1]) :-
	list_to_conjunction(X, X1),
	map_list_to_conjunction(Xs, Xs1).
map_list_to_conjunction([], []).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% The NF Transformation in nf.pl is quite slow due to the overhead
%%%% for handling 1st-order inputs.
%%%%
%%%% This is a faster implementation for propositional logic.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cnf_prop(F, M) :-
	cnf0(F, M1),
	sort(M1, M1a),
	red_unit(M1a, M2),
	red_subs(M2, M).

dnf_prop(F, M) :-
	dnf0(F, M1),
	sort(M1, M1a),
	red_unit(M1a, M2),
	red_subs(M2, M).

%%%%
%%%% input is assumed taut reduced
%%%% returns clauses that are ordsets
%%%%
cnf0_simp(M, M1) :-
	sort(M, M0),
	red_unit(M0, M2),
	red_subs(M2, M1).

cnf0_flip(M, M1) :-
	findall(C, (member(C1, M), map_mlit_complem(C1, C)), M1).

nontaut(C, D) :-
	\+ ( member(L, C),
	     mlit_complem(L , L1),
	     memberchk(L1, D)
	   ).

dnf0(F, M) :-
	cnf0(~(F), M1),
	cnf0_flip(M1, M).

cnf0((F,G), M) :-
	!,
	cnf0(F, MF),
	cnf0(G, MG),
	append(MF, MG, M).
%	ord_union(MF, MG, M).
cnf0((F;G), M) :-
	!,
	cnf0(F, MF),
	cnf0(G, MG),
	cnf0_simp(MF, MF1),
	cnf0_simp(MG, MG1),
	cnf0_multiply(MF1, MG1, M).
cnf0(~((F,G)), M) :-
	!,
	cnf0((~F;~G), M).
cnf0(~((F;G)), M) :-
	!,
	cnf0((~F,~G), M).
cnf0(~(~F), M) :-
	!,
	cnf0(F, M).
cnf0((F -> G), M) :-
	!,
	cnf0((~F ; G), M).
cnf0((F <- G), M) :-
	!,
	cnf0((F ; ~G), M).
cnf0((F <-> G), M) :-
	!,
	cnf0(((~F ; G), (F ; ~G)), M).
cnf0(~((F -> G)), M) :-
	!,
	cnf0((F , ~G), M).
cnf0(~((F <- G)), M) :-
	!,
	cnf0((~F , G), M).
cnf0(~((F <-> G)), M) :-
	!,
	cnf0(((~F , G); (F , ~G)), M).
cnf0(~true, [[]]) :-
	!.
cnf0(~false, []) :-
	!.
cnf0(true, []) :-
	!.
cnf0(false, [[]]) :-
	!.
cnf0(L, [[L]]).


cnf0_multiply(MF, MG, M) :-
	( setof(C, CF^CG^( member(CF, MF),
			   member(CG, MG),
			   nontaut(CF, CG),
			   ord_union(CF, CG, C)
		         ),
	        M) ->
	  true
	; M = []
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

red_subs(M, M1) :-
	map_sort(M, M2),
	sort_by_length(M2, M3),
	reverse(M3, M4),
	map_red_subs_c(M4, M1).

map_red_subs_c([C|M], M1) :-
	member(C1, M),
	ord_subset(C1, C),
	!,
	map_red_subs_c(M, M1).
map_red_subs_c([C|M], [C|M1]) :-
	map_red_subs_c(M, M1).
map_red_subs_c([], []).


sort_by_length(Ls, Ls1) :-
	map_key_length(Ls, LLs1),
	keysort(LLs1, LLs2),
	map_val(LLs2, Ls1).

map_key_length([X|Xs], [L-X|Xs1]) :-
	length(X, L),
	map_key_length(Xs, Xs1).
map_key_length([], []).


red_unit(M, M1) :-
	map_sort(M, M2),
	( setof(NU, U^(member([U], M2), mlit_complem(U, NU)), NUs) ->
	  true
	; NUs = []
	),
	red_unit(M2, NUs, M1).

map_sort([X|Xs], [X1|Xs1]) :-
	sort(X, X1),
	map_sort(Xs, Xs1).
map_sort([], []).

red_unit(M, [], M) :-
	!.
red_unit(M, NUs, M1) :-
	map_red_unit_c(M, NUs, NUs1, M2),
	sort(NUs1, NUs2),
	red_unit(M2, NUs2, M1).

map_red_unit_c([C|M], NUs, NUs1, [C1|M1]) :-
	ord_subtract(C, NUs, C1),
	( C1 = [U], C1 \= C ->
	  mlit_complem(U, NU),
	  NUs1 = [NU|NUs2]
	; NUs1 = NUs2
	),
	map_red_unit_c(M, NUs, NUs2, M1).
map_red_unit_c([], _, [], []).
	
mlit_atom(~A, A) :-
	!.
mlit_atom(A, A).
	
mlit_complem(~A, A) :-
	!.
mlit_complem(A, ~A).

map_mlit_complem([X|Xs], [X1|Xs1]) :-
	mlit_complem(X, X1),
	map_mlit_complem(Xs, Xs1).
map_mlit_complem([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_size([C|Cs], N) :-
	length(C, N1),
	m_size(Cs, N2),
	N is N1+N2.
m_size([], 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Propositional, i.e. treat atom terms as 0-ary predicates.
%%%% Also returns an ordset.
%%%%
m_prop_signature(M, S) :-
	( setof(A, m_prop_atom_1(M, A), S) ->
	  true
	; S = []
	).

m_prop_atom_1(M, A) :-
	member(C, M),
	member(L, C),
	mlit_atom(L, A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Stuff from here on is not tested
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

red_subsres_fwdsubs(M, M1) :-
	map_sort(M, M2),
	sort_by_length(M2, M3),
	reverse(M3, M4),
	red_subsres_1_fwdsubs(M4, M1).

red_subsres(M, M1) :-
	map_sort(M, M2),
	sort_by_length(M2, M3),
	reverse(M3, M4),
	red_subsres_1(M4, M1).

red_subs_fwd([D|M], C, M1) :-
	ord_subset(C, D),
	!,
	red_subs_fwd(M, C, M1).
red_subs_fwd([D|M], C, [D|M1]) :-
	red_subs_fwd(M, C, M1).
red_subs_fwd([], _, []).

red_subsres_1_fwdsubs(M, M1) :-
	red_subsres_step(M, [C|M2]),
	!,
	red_subs_fwd(M2, C, M3),
	red_subsres_1_fwdsubs([C|M3], M1).
red_subsres_1_fwdsubs(M, M).

red_subsres_1(M, M1) :-
	red_subsres_step(M, M2),
	!,
	red_subsres_1(M2, M1).
red_subsres_1(M, M).

red_subsres_step(M, M1) :-
	select(C, M, M2),
	select(L, C, C1),
	mlit_complem(L, L1),
	ord_add_element(C1, L1, C2),
	member(C3, M2),
	ord_subset(C3, C2),
	!,
	M1 = [C1|M2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

red_with_new_units(M, U, M1) :-
	red_unit(M, U, M1).

units(M, Us) :-
	( setof(L, member([L], M), Us) ->
	  true
	; Us = []
	).

map_uc([X|Xs], [[X]|Xs1]) :-
	map_uc(Xs, Xs1).
map_uc([], []).

mlits_atoms(Ls, As) :-
	( setof(A, L^(member(L, Ls), mlit_atom(L, A)), As) ->
	  true
	; As = []
	).

red_probe_unit(M, M1) :-
	red_unit(M, M2),
	red_probe_unit_1(M2, M1).

red_probe_unit_1(M, M1) :-
	red_probe_unit_step(M, Us),
	!,
	writeln(found(Us)),
	map_uc(Us, UsM),
	append(UsM, M, M2),
	red_with_new_units(M2, Us, M3),
	red_probe_unit_1(M3, M1).
red_probe_unit_1(M, M).

equivs(M, EQs) :-
	( setof(A-B, member([A,B], M), BCs),
	  setof(C, D^(member(D, BCs), equivs_flip(D, C)), BCsFlipped) ->
	  ord_intersection(BCs, BCsFlipped, EQs)
	; EQs = []
	).

equivs_flip(A-B, C) :-
	mlit_complem(A, A1),
	mlit_complem(B, B1),
	( A1 @< B1 ->
	  C = A1-B1
	; C = B1-A1
	).

red_probe_unit_step(M, Us) :-
	units(M, ULs),
	length(ULs, LenULs),
	mlits_atoms(ULs, UAs),
	m_prop_signature(M, S),
	ord_subtract(S, UAs, S1), %% rnd sort here
	member(A, S1),
	red_with_new_units([[A]|M], [A], MP),
	( MP = [[]] ->
	  Us = [~A]
	; units(MP, ULMPs),
	  length(ULMPs, LenULMPs),
	  ( LenULMPs > LenULs + 1 ->
	    red_with_new_units([[~A]|M], [~A], MN),
	    ( MN = [[]] ->
              Us = [A]
	    ; units(MN, ULMNs),
              length(ULMNs, LenULMNs),
	      ( LenULMNs > LenULs + 1 ->
		ord_intersection(ULMPs, ULMNs, Us1),
		ord_subtract(Us1, UAs, Us),
		Us \= []
	      ; fail
	      )
	    )
	  ; fail
	  )
	 ).

with_time_limit(Time, Goal) :-
	catch(call_with_time_limit(Time, Goal), time_limit_exceeded, fail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cnf_prop_fewsimp(F, M) :-
	cnf_prop_1(F, M1),
	red_unit(M1, M2),
	red_subs(M2, M).

cnf_prop_nosimp(F, M) :-
	cnf_prop_1(F, M1),
	sort(M1, M).

dnf_prop_nosimp(F, M) :-
	dnf_1(F, M1),
	sort(M1, M).

cnf_prop_1_simp(M, M1) :-
	sort(M, M1).

cnf_prop_1_flip(M, M1) :-
	findall(C, (member(C1, M), map_mlit_complem(C1, C)), M1).

dnf_1(F, M) :-
	cnf_prop_1(~(F), M1),
	cnf_prop_1_flip(M1, M).

cnf_prop_1((F,G), M) :-
	!,
	cnf_prop_1(F, MF),
	cnf_prop_1(G, MG),
	append(MF, MG, M).
cnf_prop_1((F;G), M) :-
	!,
	cnf_prop_1(F, MF),
	cnf_prop_1(G, MG),
	cnf_prop_1_simp(MF, MF1),
	cnf_prop_1_simp(MG, MG1),
	cnf_prop_1_multiply(MF1, MG1, M).
cnf_prop_1(~((F,G)), M) :-
	!,
	cnf_prop_1((~F;~G), M).
cnf_prop_1(~((F;G)), M) :-
	!,
	cnf_prop_1((~F,~G), M).
cnf_prop_1(~(~F), M) :-
	!,
	cnf_prop_1(F, M).
cnf_prop_1((F -> G), M) :-
	!,
	cnf_prop_1((~F ; G), M).
cnf_prop_1((F <- G), M) :-
	!,
	cnf_prop_1((F ; ~G), M).
cnf_prop_1((F <-> G), M) :-
	!,
	cnf_prop_1(((~F ; G), (F ; ~G)), M).
cnf_prop_1(~((F -> G)), M) :-
	!,
	cnf_prop_1((F , ~G), M).
cnf_prop_1(~((F <- G)), M) :-
	!,
	cnf_prop_1((~F , G), M).
cnf_prop_1(~((F <-> G)), M) :-
	!,
	cnf_prop_1(((~F , G); (F , ~G)), M).
cnf_prop_1(~true, [[]]) :-
	!.
cnf_prop_1(~false, []) :-
	!.
cnf_prop_1(true, []) :-
	!.
cnf_prop_1(false, [[]]) :-
	!.
cnf_prop_1(L, [[L]]).


cnf_prop_1_multiply(MF, MG, M) :-
	( setof(C, CF^CG^( member(CF, MF),
			   member(CG, MG),
			   nontaut(CF, CG),
			   ord_union(CF, CG, C)
		         ),
	        M) ->
	  true
	; M = []
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Prime Implicants, Very Naive Implementation
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% The output matrix contains the set of prime implicants/implicates
%%%% of the input matrix (i.e. the output matrix is closed under
%%%% resolution and subsumption).		    
%%%% 
m_prime(M, M1) :-
	red_taut(M, M2),
	red_subs(M2, M3),
	map_sort(M3, M4),
	m_prime_1(M4, M1).

res(C1, C2, R) :-
	sort(C1, C1S),
	sort(C2, C2S),
	c_flip(C1S, FC1S),
	resolvent_1(FC1S, C2S, R).

resolvent_1(FlippedC1, C2, R) :-
	ord_intersection(FlippedC1, C2, [Upon]),
	!,
	c_flip(FlippedC1, C1),
	ord_union(C1, C2, R1),
	mlit_complem(Upon, Upon1),
	sort([Upon, Upon1], Upon2),
	ord_subtract(R1, Upon2, R).
	

c_flip(C, C1) :-
	c_flip_1(C, C2),
	sort(C2, C1).

c_flip_1([~A|C],[A|C1]) :-
	!,
	c_flip_1(C, C1).
c_flip_1([A|C],[~A|C1]) :-
	!,
	c_flip_1(C, C1).
c_flip_1([], []).

c_taut(C) :-
	member(~A, C),
	memberchk(A, C),
	!.

cm_subsumed(C, M) :-
	member(D, M),
	ord_subset(D, C),
	!.

mc_rem_subsumed([C|M], D, M1) :-
	ord_subset(D, C),
	!,
	mc_rem_subsumed(M, D, M1).
mc_rem_subsumed([C|M], D, [C|M1]) :-
	mc_rem_subsumed(M, D, M1).
mc_rem_subsumed([], _, []).


m_prime_1(M, M1) :-
	member(C, M),
	c_flip(C, FC),
	member(D, M),
	resolvent_1(FC, D, R),
	\+ cm_subsumed(R, M),
	!,
	mc_rem_subsumed(M, R, M2),
	m_prime_1([R|M2], M1).
m_prime_1(M, M).

red_taut([C|M], M1) :-
	c_taut(C),
	!,
	red_taut(M, M1).
red_taut([C|M], [C|M1]) :-
	red_taut(M, M1).
red_taut([], []).	