%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2016, 2019 Christoph Wernhard
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

:- module(simp_fol, [simp_form/3,
		     simp_form/4,
		     simpinfo/2,
		     simp_fol_fast/2,
		     simp_fol_c/2,
		     simp_fol_d/2,
		     simp_fol_c4/2,
		     simp_fol_d4/2,
		     simp_fol_c5/2,
		     simp_fol_d5/2,
		     simp_fol_c6/2,
		     simp_fol_d6/2,
		     simp_ttlogform_fast/2,
		     inline_eq/2,
		     inline_eq/5,
		     quants_gather/2,
		     dist_eqcon/2,
		     align_unitpref/2,
		     prop_units/2,
		     pullout/7,
		     negate_nnf/2,
		     nnf_size/2,
		     topex_outwards/2,
		     logform_remove_void_quantifiers/2]).

:- use_module(swilib(err)).
:- use_module(swilib(pretty)).
:- use_module(logop_fol).
:- use_module(tform).
:- use_module(auxiliary_fol).
:- use_module(utils_fol).
:- use_module(prettysymbols_fol).
:- use_module(unskclausal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% pullout(+F, +Q, +P, +Arity, +D, -F1, -Q1) :-
%%%%
%%%% F: is a NNF
%%%%
%%%% P: Specifies the quantified predicate variable to pull out and its
%%%% polarity
%%%%
%%%% A: Dominating universal variables (i.e. universal variables in whose
%%%% scope we are); perhaps useful for e.g. Henkin quantifiers where
%%%% dependencies do not have to be linearized
%%%%
%%%% Q1: Quantifier prefix
%%%%
%%%% F1: Result formulas F1 have the shape:
%%%% (all([X], (PX;CX)), D)
%%%%
% pullout(F, Q, P, N, A, F1, Q1) :-
% 	dbg(pullout(F, Q, P, N, A, F1, Q1)),
% 	fail.

pullout(all(V, F), Q, P, N, A, F1, Q1) :-
	var(V),
	!,
	pullout(all([V], F), Q, P, N, A, F1, Q1).
pullout(all([V|Vs], F), Q, P, N, A, F1, [all(V)|Q1]) :-
	!,
	pullout(all(Vs, F), Q, P, N, [V|A], F1, Q1).
pullout(all([], F), Q, P, N, A, F1, Q1) :-
	!,
	pullout(F, Q, P, N, A, F1, Q1).
pullout(ex(V, F), Q, P, N, A, F1, Q1) :-
	var(V),
	!,
	pullout(ex([V], F), Q, P, N, A, F1, Q1).
pullout(ex([V|Vs], F), Q, P, N, A, F1, [ex(V, A)|Q1]) :-
	!,
	pullout(ex(Vs, F), Q, P, N, A, F1, Q1).
pullout(ex([], F), Q, P, N, A, F1, Q1) :-
	!,
	pullout(F, Q, P, N, A, F1, Q1).
pullout((F,G), Q, P, N, A, (all(X, (PX;CX)), D), Q1) :-
	!,
	pullout(G, Q, P, N, A, (all(X, (PX;CXG)), DG), Q2),
	pullout(F, Q2, P, N, A, (all(X, (PX;CXF)), DF), Q1),
	logform_conjoin(CXF, CXG, CX),
	logform_conjoin(DF, DG, D).
pullout((F;G), Q, P, N, A, (all(X, (PX;CX)), D), Q1) :-
	!,
	pullout(G, Q, P, N, A, (all(X, (PX;CXG)), DG), Q2),
	pullout(F, Q2, P, N, A, (all(X, (PX;CXF)), DF), Q3),
	logform_disjoin(DF, DG, D1),
	tnnfpl_copy_bound_vars(CXF, CXF1),
	tnnfpl_copy_bound_vars(CXG, CXG1),
	tnnfpl_copy_bound_vars(DF, DF1),
	tnnfpl_copy_bound_vars(DG, DG1),
	logform_disjoin(CXF1, DG1, K1),
	logform_disjoin(CXG1, DF1, K2),
	logform_conjoin(K1, K2, CX0),
	%%
	%% Handle PCF v PCG, part I
	tnnfpl_copy_bound_vars(CXF, CXF2),
	tnnfpl_copy_bound_vars(CXG, CXG2),
	copy_given_vars(X, CXF2, VF, CXF3),
	copy_given_vars(X, CXG2, VG, CXG3),
	logform_disjoin(CXF3, CXG3, CX3),
	mk_tuple_and_quants(N,UT,VU),
	mk_tuple_and_quants(N,VFT,VF),
	mk_tuple_and_quants(N,VGT,VG),
	logform_disjoin((UT = VFT ; UT = VGT), CX3, D2),
	logform_conjoin(D2, D1, D),
	%%
	%% Handle PCF v PCG, part II
	tnnfpl_copy_bound_vars(CXF3, CXF4), %%%
	tnnfpl_copy_bound_vars(CXG3, CXG4),
	logform_disjoin(CXF4, CXG4, CX4),
	mk_tuple_and_quants(N, TX, X),
	logform_disjoin(~(TX = UT), CX4, CX5),
	logform_conjoin(CX5, CX0, CX),
	append(VF, VG, VFG),
	append(VFG, A, VDep),
	map_ex_dep(VU, VDep, ExDep),
	map_all(VFG, AllVs),
	append(AllVs, ExDep, AEDep),
	append(Q3, AEDep, Q1).
pullout(L, Q, +(P), N, _, (all(X, (P/TX ; ~(TX = T))), true), Q) :-
	is_pos_lit_with_pred(L, P),
	!,
	mk_tuple_and_quants(N, TX, X),
	lit_args(L, T).
pullout(L, Q, -(P), N, _, (all(X, (~(P/TX) ; ~(TX = T))), true), Q) :-
	is_neg_lit_with_pred(L, P),
	!,
	mk_tuple_and_quants(N, TX, X),
	lit_args(L, T).
pullout(L, Q, +(P), N, _, (all(X, (P/TX ; true)), L), Q) :-
	!,
	mk_tuple_and_quants(N, TX, X). 
pullout(L, Q, -(P), N, _, (all(X, (~(P/TX) ; true)), L), Q) :-
	mk_tuple_and_quants(N, TX, X).

mk_tuple_and_quants(N, TX, X) :-
	tuple_functor(T),
	functor(TX, T, N),
	TX =.. [_|X].

map_ex_dep([X|Xs], Y1, [ex(X,Y1)|Xs1]) :-
	map_ex_dep(Xs, Y1, Xs1).
map_ex_dep([], _, []).

map_all([X|Xs], [all(X)|Xs1]) :-
	map_all(Xs, Xs1).
map_all([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Simplifications
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%% CHECK: proper handling of free variables
%%%% - do not allow to instantiate them (the more safe option)
%%%% - ommit an inner quantifier if its var has been instantiated to a free
%%%%

%%%%
%%%% Inlines equations of a NNF in cases where this can be done by only
%%%% instantiating universally quantified vars. Also performs some truth value
%%%% simplification and simplification of equivalences with identical
%%%% arguments.
%%%%
inline_eq(F, F1) :-
	ieq(F, [], [], [], F1).

inline_eq(F, A, B, C, F1) :-
	ieq(F, A, B, C, F1).

trim_ieq_vars(V, C, F, V1) :-
	term_variables(V, V2), %% to remove constants and duplicates
	subtractq(V2, C, V3),
	tnnfpl_free_prolog_vars(F, V4),
	intersectionq(V3, V4, V1).

%%%%
%%%% +A: universal variables available for prolog-binding
%%%%     (unless already bound to member of B or constant)
%%%% +B: variables that are fol-bound and can be used for a single
%%%%     prolog-binding
%%%% +C: variables that are fol-bound
%%%%
ieq((F;G), A, B, C, FG) :-
	!,
	ieq(F, A, B, C, F1),
	ieq(G, A, B, C, G1),
	logform_disjoin(F1, G1, FG).
ieq((F,G), A, B, C, FG) :-
	!,
	term_variables(F, VF),
	term_variables(G, VG),
	subtractq(A, VG, AF),
	subtractq(A, VF, AG),
	intersectionq(VF, VG, VS),
	unionq(B, VS, B1),
	ieq(F, AF, B1, C, F1),
	ieq(G, AG, B1, C, G1),
	logform_conjoin(F1, G1, FG).
ieq(all(V, F), A, B, C, F1) :-
	!,
	logupon_list(V, V1),
	unionq(V1, A, A1),
	unionq(V1, C, C1),
	ieq(F, A1, B, C1, F2),
	trim_ieq_vars(V1, C, F2, V2),
	logform_allquant(V2, F2, F1).
ieq(ex(V, F), _, B, C, F1) :-
	!,
	logupon_list(V, V1),
	unionq(V1, B, B1),
	unionq(V1, C, C1),
	ieq(F, [], B1, C1, F2),
	logform_exquant(V1, F2, F1).
ieq(~(X = Y), _, _, _, false) :-
	X == Y,
	!.
ieq(~(X = Y), A, B, C, F) :-
	\+ var(X),
	\+ var(Y),
	tuple_functor(T),
	X =.. [T|Xs],
	Y =.. [T|Ys],
	expand_neq_tuple(Xs, Ys, F1),
	!,
	ieq(F1, A, B, C, F).
ieq(~(X = Y), A, B, _, false) :-
  	( var(X), memberq(X, A), \+ memberq(X, B) ->
	  ( var(Y), memberq(Y, A) -> true
	  ; var(Y), memberq(Y, B) -> true
	  ; ground(Y)
	  )
	; var(Y), memberq(Y, A), \+ memberq(Y, B) ->
	  ( var(X), memberq(X, A) -> true
	  ; var(X), memberq(X, B) -> true
	  ; ground(X)
	  )
	),
	unify_with_occurs_check(X, Y),
 	!.
ieq(~(X = Y), _, _, _, true) :-
	get_conf(fol_equality_mode, herbrand),
	\+ unify_with_occurs_check(X, Y),
	!.
ieq(~(X = Y), _, _, _, ~(Y = X)) :-
	Y @< X,
	!.
ieq(X = Y, _, _, _, true) :-
	X == Y,
	!.
ieq(X = Y, A, B, C, F) :-
	\+ var(X),
	\+ var(Y),
	tuple_functor(T),
	X =.. [T|Xs],
	Y =.. [T|Ys],
	expand_eq_tuple(Xs, Ys, F1),
	!,
	ieq(F1, A, B, C, F).
ieq(X = Y, _, _, _, Y = X) :-
	Y @< X,
	!.
ieq(F, _, _, _, F).

expand_neq_tuple([X], [Y], ~(X = Y)) :-
	!.
expand_neq_tuple([X|Xs], [Y|Ys], (~(X = Y) ; F)) :-
	expand_neq_tuple(Xs, Ys, F).
expand_neq_tuple([], [], false).

expand_eq_tuple([X], [Y], X = Y) :-
	!.
expand_eq_tuple([X|Xs], [Y|Ys], (X = Y , F)) :-
	expand_eq_tuple(Xs, Ys, F).
expand_eq_tuple([], [], true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Quants Gather (A bit like Gustafsson's 3.3, but without multiplying)
%%%%
%%%% Antriprenexing, combined with some associativity/commutativity
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% quants_gather(F, _) :-
%%%% 	pp(F), nl, fail.
%%%% 

quants_gather(ex(X, F), F1) :-
	var(X),
	!,
	quants_gather(ex([X], F), F1).
quants_gather(all(X, F), F1) :-
	var(X),
	!,
	quants_gather(all([X], F), F1).
quants_gather(all(X, (F, G)), FG) :-
	!,
	copy_given_vars(X, G, X1, G1),
	quants_gather((all(X, F), all(X1, G1)), FG).
%%%% antiprenex
quants_gather(ex(X, (F; G)), FG) :-
 	!,
	FG = ex(X, FG1),
 	quants_gather((F; G), FG1).
%%%% antiprenex
quants_gather((ex([X|Xs], F) ; ex([Y|Ys], G)), FG) :-
	!,
	Y = X,
	quants_gather((ex([X], (ex(Xs,F); ex(Ys,G)))), FG).
%%%% inwards
% quants_gather(ex(X, (F; G)), FG) :-
% 	!,
% 	copy_given_vars(X, G, X1, G1),
% 	quants_gather((ex(X, F); ex(X1, G1)), FG).
quants_gather(ex([X|Xs], F), F1) :-
	!,
	quants_gather(ex(Xs, F), F2),
	split_conjuncts_with_var(F2, X, FX, FR),
	( FX = true ->
	  F1 = FR
	; FR = true ->
	  F1 = ex([X], FX)
	; F1 = (ex([X], FX), FR)
	).
quants_gather(ex([], F), F1) :-
	!,
	quants_gather(F, F1).
quants_gather(all([X|Xs], F), F1) :-
	!,
	quants_gather(all(Xs, F), F2),
	split_disjuncts_with_var(F2, X, FX, FR),
	( FX = false ->
	  F1 = FR
	; ( FX = (_, _) ->
	    quants_gather(all([X], FX), FX1)
	  ; FX1 = all([X], FX)
	  ),
	  ( FR = false ->
	    F1 = FX1
	  ; F1 = (FX1; FR)
	  )
	).
quants_gather(all([], F), F1) :-
	!,
	quants_gather(F, F1).
quants_gather((F;G), (F1;G1)) :-
	!,
	quants_gather(F, F1),
	quants_gather(G, G1).
quants_gather((F,G), (F1,G1)) :-
	!,
	quants_gather(F, F1),
	quants_gather(G, G1).
%%%%
%%%% handling ex2 in the same way as ex
%%%%
quants_gather(ex2(X, F), F1) :-
	var(X),
	!,
	quants_gather(ex2([X], F), F1).
%%%% antiprenex
quants_gather(ex2(X, (F; G)), FG) :-
 	!,
	FG = ex2(X, FG1),
 	quants_gather((F; G), FG1).
%%%% antiprenex
quants_gather((ex2([X|Xs], F) ; ex2([Y|Ys], G)), FG) :-
	!,
	Y = X,
	quants_gather((ex2([X], (ex2(Xs,F); ex2(Ys,G)))), FG).
quants_gather(ex2([X|Xs], F), F1) :-
	!,
	quants_gather(ex2(Xs, F), F2),
	( term_variables(X, [Var]) ->
	  true
	; err('Invalid predicate variable specifier: ~q', [X])
	),
	split_conjuncts_with_var(F2, Var, FX, FR),
	( FX = true ->
	  F1 = FR
	; FR = true ->
	  F1 = ex2([X], FX)
	; F1 = (ex2([X], FX), FR)
	).
quants_gather(ex2([], F), F1) :-
	!,
	quants_gather(F, F1).
quants_gather(X, X).

split_disjuncts_with_var((F; G), X, FGX, FGR) :-
	!,
	split_disjuncts_with_var(F, X, FX, FR),
	split_disjuncts_with_var(G, X, GX, GR),
	logform_disjoin(FX, GX, FGX),
	logform_disjoin(FR, GR, FGR).
split_disjuncts_with_var(F, X, F, false) :-
	contains_var(X, F),
	!.
split_disjuncts_with_var(F, _, false, F).

split_conjuncts_with_var((F, G), X, FGX, FGR) :-
	!,
	split_conjuncts_with_var(F, X, FX, FR),
	split_conjuncts_with_var(G, X, GX, GR),
	logform_conjoin(FX, GX, FGX),
	logform_conjoin(FR, GR, FGR).
split_conjuncts_with_var(F, X, F, true) :-
	contains_var(X, F),
	!.
split_conjuncts_with_var(F, _, true, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Distribute Conjunctions of Equalities
%%%%
%%%% Specialized to a particular form that seem introduced in presence of
%%%% extensions "p(a), p(b), p(c)". This should precede inline_eq on
%%%% the elimination result.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dist_eqcon(all(X, (((~(Y = Z)), F) ; G)), FG1) :-
	term_variables(X, [Y1]),
	Y == Y1,
	!,
	tnnfpl_copy_bound_vars(F, F1),
	tnnfpl_copy_bound_vars(G, G1),
	copy_given_vars([Y1], F1-G1, [Y2], F2-G2),
	dist_eqcon(( all([Y], ((~(Y = Z) ; G))),
		     all([Y2], (F2 ; G2))),
		   FG1).
dist_eqcon(all(X, (((~(Z = Y)), F) ; G)), FG1) :-
	term_variables(X, [Y1]),
	Y == Y1,
	!,
	tnnfpl_copy_bound_vars(F, F1),
	tnnfpl_copy_bound_vars(G, G1),
	copy_given_vars([Y1], F1-G1, [Y2], F2-G2),
	dist_eqcon(( all([Y], ((~(Y = Z) ; G))),
		     all([Y2], (F2 ; G2))),
		   FG1).
dist_eqcon((F;G), (F1;G1)) :-
	!,
	dist_eqcon(F, F1),
	dist_eqcon(G, G1).
dist_eqcon((F,G), (F1,G1)) :-
	!,
	dist_eqcon(F, F1),
	dist_eqcon(G, G1).

dist_eqcon(all(X, F), all(X, F1)) :-
	!,
	dist_eqcon(F, F1).
dist_eqcon(ex(X, F), ex(X, F1)) :-
	!,
	dist_eqcon(F, F1).
dist_eqcon(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Align Unitpref
%%%%
%%%% Two purposes:
%%%% - heuristics to improve applicability of dist_eqcon
%%%% - basis for inward unit propagation
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

align_unitpref((F,G), FG) :-
	!,
	andseq_to_list((F,G), FG1),
	map_align_unitpref(FG1, FG2),
	sort_unitpref(FG2, FG3),
	list_to_andseq(FG3, FG).
align_unitpref((F;G), FG) :-
	!,
	orseq_to_list((F;G), FG1),
	map_align_unitpref(FG1, FG2),
	sort_unitpref(FG2, FG3),
	list_to_orseq(FG3, FG).
align_unitpref(all(X, F), F1) :-
	!,
	align_unitpref(F, F2),
	logform_allquant(X, F2, F1).
align_unitpref(ex(X, F), F1) :-
	!,
	align_unitpref(F, F2),
	logform_exquant(X, F2, F1).
align_unitpref(F, F).

map_align_unitpref([X|Xs], [X1|Xs1]) :-
	align_unitpref(X, X1),
	map_align_unitpref(Xs, Xs1).
map_align_unitpref([], []).

sort_unitpref(Fs, Fs1) :-
	map_add_unitpref_key(Fs, KFs),
	sort(KFs, KFs1),
	map_val(KFs1, Fs1).

add_unitpref_key(true, 5-true) :-
	!.
add_unitpref_key(false, 6-false) :-
	!.
add_unitpref_key(~(X = Y), 10-(~(X = Y))) :-
	var(X),
	!.
add_unitpref_key((~(X = Y),F), 25-((~(X = Y),F))) :-
	var(X),
	!.
add_unitpref_key(~(X = Y), 20-(~(X = Y))) :-
	!.
add_unitpref_key(X = Y, 38-(X = Y)) :-
	!.
add_unitpref_key((F,G), 55-(F,G)) :-
	!.
add_unitpref_key((F;G), 50-(F;G)) :-
	!.
add_unitpref_key(all(X,F), 60-all(X,F)) :-
	!.
add_unitpref_key(ex(X,F), 70-ex(X,F)) :-
	!.
add_unitpref_key(~A, 35-(~A)) :-
	!.
add_unitpref_key(A, 40-A) :-
	!.

map_add_unitpref_key([X|Xs], [X1|Xs1]) :-
	add_unitpref_key(X, X1),
	map_add_unitpref_key(Xs, Xs1).
map_add_unitpref_key([], []).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).


prop_units(F, F1) :-
	prop_units(F, [], F1).

% prop_units(F, U, _) :-
% 	dbg(pu(F,U)),
% 	fail.

prop_units((false,_), _, false) :-
	!.
prop_units((true,F), U, F1) :-
	!,
	prop_units(F, U, F1).
prop_units((true;_), _, true) :-
	!.
prop_units((false;F), U, F1) :-
	!,
	prop_units(F, U, F1).
prop_units((F,G), Us, FG) :-
	unit(F, NF),
	!,
	( memberq(F, Us) ->
	  prop_units(G, Us, FG)
	; memberq(NF, Us) ->
	  FG = false
	; prop_units(G, [F|Us], G1),
	  logform_conjoin(F, G1, FG)
	).
prop_units((F,G), Us, FG) :-
	!,
	prop_units(F, Us, F1),
	prop_units(G, Us, G1),
	logform_conjoin(F1, G1, FG).
	
prop_units((F;G), Us, FG) :-
	unit(F, NF),
	!,
	( memberq(NF, Us) ->
	  prop_units(G, Us, FG)
	; memberq(F, Us) ->
	  FG = true
	; prop_units(G, [NF|Us], G1),
	  logform_disjoin(F, G1, FG)
	).
prop_units((F;G), Us, FG) :-
	!,
	prop_units(F, Us, F1),
	prop_units(G, Us, G1),
	logform_disjoin(F1, G1, FG).
prop_units(all(X, F), Us, F1) :-
	!,
	prop_units(F, Us, F2),
	logform_allquant(X, F2, F1).
prop_units(ex(X, F), Us, F1) :-
	!,
	prop_units(F, Us, F2),
	logform_exquant(X, F2, F1).
prop_units(F, Us, F1) :-
	unit(F, NF),
	!,
	( memberq(F, Us) ->
	  F1 = true
	; memberq(NF, Us) ->
	  F1 = false
	; F1 = F
	).
prop_units(F, _, F).

unit(~(A), A) :- !.
unit((_,_), _) :- !, fail.
unit((_;_), _) :- !, fail.
unit(all(_,_), _) :- !, fail.
unit(ex(_,_), _) :- !, fail.
unit(A, ~(A)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nnf_size((A, B), N) :-
	!,
	nnf_size(A, N1),
	nnf_size(B, N2),
	N is N1 + N2.
nnf_size((A; B), N) :-
	!,
	nnf_size(A, N1),
	nnf_size(B, N2),
	N is N1 + N2.
nnf_size(all(_, F), N) :-
	!,
	nnf_size(F, N).
nnf_size(ex(_, F), N) :-
	!,
	nnf_size(F, N).
nnf_size(all2(_, F), N) :-
	!,
	nnf_size(F, N).
nnf_size(ex2(_, F), N) :-
	!,
	nnf_size(F, N).
nnf_size(_, 1).

negate_nnf((F, G), (F1; G1)) :-
	!,
	negate_nnf(F, F1),
	negate_nnf(G, G1).
negate_nnf((F; G), (F1, G1)) :-
	!,
	negate_nnf(F, F1),
	negate_nnf(G, G1).
negate_nnf(ex(V, F), all(V, F1)) :-
	!,
	negate_nnf(F, F1).
negate_nnf(all(V, F), ex(V, F1)) :-
	!,
	negate_nnf(F, F1).
negate_nnf(ex2(V, F), all2(V, F1)) :-
	!,
	negate_nnf(F, F1).
negate_nnf(all2(V, F), ex2(V, F1)) :-
	!,
	negate_nnf(F, F1).
negate_nnf(~A, A) :-
	!.
negate_nnf(A, ~A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tuple_functor(t).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lit_args(~(_/Args), Args) :-
	!.
lit_args(_/Args, Args).

is_pos_lit_with_pred(L, P) :-
	compound(L),
	L \= ~_,
	arg(1, L, PL),
	PL == P.

is_neg_lit_with_pred(~A, P) :-
	compound(A),
	arg(1, A, PL),
	PL == P.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% DLS Preparation: Move Toplevel Ex Outwards
%%%%
%%%%  used in elim_fol and unskolemize
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Note: ex2 is not properly moved outside by this implementation (the
%%%% interplay with ex would have to be implemented). It is basically
%%%% intended to be invoked on formulas without occurrences of ex2.
%%%% 
topex_outwards(F, F1) :-
	topex_outwards_1(F, F1).

topex_outwards_1((F,G), H) :-
	!,
	topex_outwards_1(F, F1),
	topex_outwards_1(G, G1),
	conjoin_topex(F1, G1, H).
topex_outwards_1((F;G), H) :-
	!,
	topex_outwards_1(F, F1),
	topex_outwards_1(G, G1),
	disjoin_topex(F1, G1, H).
topex_outwards_1(all(X, F), all(X, F)) :-
	!.
topex_outwards_1(ex(X, F), F1) :-
	!,
	topex_outwards_1(F, F2),
	exmerge_topex(F2, X, F1).
topex_outwards_1(ex2(X, F), F1) :-
	!,
	topex_outwards_1(F, F2),
	ex2merge_topex(F2, X, F1).
topex_outwards_1(F, F).

exmerge_topex(ex(Y, F), X, ex(XY, F)) :-
	!,
	append(X, Y, XY).
exmerge_topex(F, X, ex(X, F)).

ex2merge_topex(ex2(Y, F), X, ex2(XY, F)) :-
	!,
	append(X, Y, XY).
ex2merge_topex(F, X, ex2(X, F)).

conjoin_topex(ex(X,F), ex(Y,G), ex(XY, (F,G))) :-
	!,
	append(X, Y, XY).
conjoin_topex(ex(X,F), G, ex(X, (F,G))) :-
	!.
conjoin_topex(F, ex(X,G), ex(X, (F,G))) :-
	!.
conjoin_topex(F, G, (F,G)).

exmerge_topex_disjoin([X|Xs], [Y|Ys], [X|Zs]) :-
	!,
	Y = X,
	exmerge_topex_disjoin(Xs, Ys, Zs).
exmerge_topex_disjoin([], Ys, Ys) :-
	!.
exmerge_topex_disjoin(Xs, [], Xs).

disjoin_topex(ex(X,F), ex(Y,G), ex(XY, (F;G))) :-
	!,
	exmerge_topex_disjoin(X, Y, XY).
disjoin_topex(ex(X,F), G, ex(X, (F;G))) :-
	!.
disjoin_topex(F, ex(X,G), ex(X, (F;G))) :-
	!.
disjoin_topex(F, G, (F;G)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% A fast simplification of first-order formulas
%%%%
%%%% - also applicable to the tform format
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simp_fol_fast(F, F1) :-
	logform_process_subforms(F, simp_fol_tv_step, F1).

simp_ttlogform_fast(F, F1) :-
	simp_fol_fast(F, F1).

simp_fol_tv_step(F, F1) :-
	simp_fol_tv_step_1(F, F1),
	!.
simp_fol_tv_step(F, F).
			 
%%%
simp_fol_tv_step_1(P/t(X,Y), true) :- P == (=), X == Y. %% for tform
simp_fol_tv_step_1((X=Y), true) :- X == Y.
%%
simp_fol_tv_step_1(~true, false).
simp_fol_tv_step_1(~false, true).
%%
simp_fol_tv_step_1((false, _), false).
simp_fol_tv_step_1((_, false), false).
simp_fol_tv_step_1((true, X), X).
simp_fol_tv_step_1((X, true), X).
simp_fol_tv_step_1((true; _), true).
simp_fol_tv_step_1((_; true), true).
simp_fol_tv_step_1((false; X), X).
simp_fol_tv_step_1((X; false), X).
%%
simp_fol_tv_step_1(all(_, true), true).
simp_fol_tv_step_1(ex(_, true), true).
simp_fol_tv_step_1(all(_, false), false).
simp_fol_tv_step_1(ex(_, false), false).
%%
simp_fol_tv_step_1(all2(_, true), true).
simp_fol_tv_step_1(ex2(_, true), true).
simp_fol_tv_step_1(all2(_, false), false).
simp_fol_tv_step_1(ex2(_, false), false).
%%
simp_fol_tv_step_1((_-> true), true).
simp_fol_tv_step_1((true -> X), X).
simp_fol_tv_step_1((X -> false), ~X).
simp_fol_tv_step_1((false -> _), true).
%%
simp_fol_tv_step_1((X <- true), X).
simp_fol_tv_step_1((true <- _), true).
simp_fol_tv_step_1((_ <- false), true).
simp_fol_tv_step_1((false <- X), ~X).
%%
simp_fol_tv_step_1((X, (Y, Z)), (X, Z)) :- eq_simp(X, Y).
simp_fol_tv_step_1((X; (Y; Z)), (X; Z)) :- eq_simp(X, Y).
simp_fol_tv_step_1((X, Y), X) :- eq_simp(X, Y).
simp_fol_tv_step_1((X; Y), X) :- eq_simp(X, Y).
%%
simp_fol_tv_step_1((X, (~Y, _)), false) :- eq_simp(X, Y).
simp_fol_tv_step_1((~X, (Y, _)), false) :- eq_simp(X, Y).
simp_fol_tv_step_1((X; (~Y; _)), true) :- eq_simp(X, Y).
simp_fol_tv_step_1((~X; (Y; _)), true) :- eq_simp(X, Y).
simp_fol_tv_step_1((X, ~Y), false) :- eq_simp(X, Y).
simp_fol_tv_step_1((~X, Y), false) :- eq_simp(X, Y).
simp_fol_tv_step_1((X; ~Y), true) :- eq_simp(X, Y).
simp_fol_tv_step_1((~X; Y), true) :- eq_simp(X, Y).
%%
simp_fol_tv_step_1((X -> Y), true) :- eq_simp(X, Y).
simp_fol_tv_step_1((X <- Y), true) :- eq_simp(X, Y).
simp_fol_tv_step_1((X -> ~Y), ~X) :- eq_simp(X, Y).
simp_fol_tv_step_1((~X <- Y), ~X) :- eq_simp(X, Y).
simp_fol_tv_step_1((~X -> Y), X) :- eq_simp(X, Y).
simp_fol_tv_step_1((X <- ~Y), X) :- eq_simp(X, Y).
%%
simp_fol_tv_step_1((X <-> true), X).
simp_fol_tv_step_1((true <-> X), X).
simp_fol_tv_step_1((X <-> false), ~X).
simp_fol_tv_step_1((false <-> X), ~X).
simp_fol_tv_step_1((X <-> Y), true) :- eq_simp(X, Y).
simp_fol_tv_step_1((X <-> ~Y), false) :- eq_simp(X, Y).
simp_fol_tv_step_1((~X <-> Y), false) :- eq_simp(X, Y).
%%
simp_fol_tv_step_1(~(~X), X).

eq_simp(X, Y) :-
	X == Y,
	!.
eq_simp(A=B, C=D) :-
	A == D,
	B == C,
	!.
eq_simp(~(A=B), ~(C=D)) :-
	A == D,
	B == C,
	!.
eq_simp(P/t(A,B), Q/t(C,D)) :-
	P == (=),
	Q == (=),
	A == D,
	B == C,
	!.
eq_simp(~(P/t(A,B)), ~(Q/t(C,D))) :-
	P == (=),
	Q == (=),
	A == D,
	B == C,
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% try_matrix_to_form(M, Fallback, F) :-
% 	catch( matrix_to_form(M, F),
% 	       nf_unskolemization_failure,
% 	       ( info(10, 'Simp: skipping a simplification due to \c
% 	       failure of the implemented unskolemization'),
% 		 F = Fallback )).

try_matrix_to_form(M, Fallback, F) :-
	catch( m_unskolemize(M, F),
	       clausal_unskolemization_failure,
	       ( info(10, 'Simp: skipping a simplification due to \c
	       failure of the implemented unskolemization'),
		 F = Fallback )).

simp_fol_c(F, F1) :-
	cnf1(F, F2),
	try_matrix_to_form(F2, F, F1).

simp_fol_c4(F, F1) :-
	cnf4(F, F2),
	try_matrix_to_form(F2, F, F1).

simp_fol_c5(F, F1) :-
	cnf5(F, F2),
	try_matrix_to_form(F2, F, F1).

simp_fol_c6(F, F1) :-
	cnf6(F, F2),
	try_matrix_to_form(F2, F, F1).

simp_fol_d(F, F1) :-
	cnf1(~F, F2),
	try_matrix_to_form(F2, ~F, F3),
	logform_negate(F3, F1).

simp_fol_d4(F, F1) :-
	cnf4(~F, F2),
	matrix_to_form(F2, F3),
	try_matrix_to_form(F2, ~F, F3),
	logform_negate(F3, F1).

simp_fol_d5(F, F1) :-
	cnf5(~F, F2),
	try_matrix_to_form(F2, ~F, F3),
	logform_negate(F3, F1).

simp_fol_d6(F, F1) :-
	cnf6(~F, F2),
	try_matrix_to_form(F2, ~F, F3),
	logform_negate(F3, F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_remove_void_quantifiers(F, F1) :-
	logform_clean_vars(F, F2),
	logform_process_subforms(F2, rm_void_quantifiers_1, F1).

rm_void_quantifiers_1(F, F1) :-
	functor(F, Q, 2),
	logop_quantifier(Q),
	!,
	arg(2, F, F2),
	arg(1, F, Upon),
	logupon_list(Upon, Upon1),
	rm_vq_2(Upon1, F2, Upon3),
	( Upon3 = [] -> F1 = F2
	; Upon3 = [X] -> F1 =.. [Q,X,F2]
	; F1 =.. [Q,Upon3,F2]
	).
rm_void_quantifiers_1(F, F).

rm_vq_2([X|Xs], F, Xs1) :-
	( contains_var(X, F) ->
	  Xs1 = [X|Xs2]
	; logform_enum_atoms(F, A),
	  functor(A, Functor, _),
	  X == Functor ->
	  Xs1 = [X|Xs2]
	; Xs1 = Xs2
	),
	rm_vq_2(Xs, F, Xs2).
rm_vq_2([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% simp_form/3
%%%% simp_form/4
%%%% 
%%%% Interface to formula simplifications. Outputs via info/3 given
%%%% information about the contex of the simplification and computed
%%%% information about the formula size before and after simplifications
%%%% steps.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simp_form(F, Simp, F1) :-
	simp_form(F, Simp, 'unspecified', F1).

simp_form(F, [], _, F) :-
	!.
simp_form(F, [S|S1], I, F1) :-
	!,
	simp_form(F, S, I, F2),
	simp_form(F2, S1, I, F1).
simp_form(F, Simp, Info, F1) :-
	logform_size(F, S0),
	info(10, 'Simp: ~w ~w, input size: ~w', [Simp, Info, S0]),
	( Simp = none ->
	  F1 = F
	; simp_fol_fast(F, F2),
	  ( Simp = c4 -> simp_fol_c4(F2, F3)
	  ; Simp = d4 -> simp_fol_d4(F2, F3)
	  ; Simp = c5 -> simp_fol_c5(F2, F3)
	  ; Simp = d5 -> simp_fol_d5(F2, F3)
	  ; Simp = c6 -> simp_fol_c6(F2, F3)
	  ; Simp = d6 -> simp_fol_d6(F2, F3)
	  ; Simp = c5i ->
	    simp_fol_c5(F2, F4),
	    logform_size(F2, S2),
	    logform_size(F4, S4),
	    ( S4 > S2 -> F3 = F2 ; F3 = F4 )
	  ; Simp = d5i ->
	    simp_fol_d5(F2, F4),
	    logform_size(F2, S2),
	    logform_size(F4, S4),
	    ( S4 > S2 -> F3 = F2 ; F3 = F4 )
	  ; Simp = cd5i ->
	    simp_fol_c5(F2, F4),
	    simp_fol_d5(F2, F5),
	    logform_size(F2, S2),
	    logform_size(F4, S4),
	    logform_size(F5, S5),
	    ( S4 > S5 ->
	      ( S5 > S2 -> F3 = F2 ; F3 = F5 )
	    ; ( S4 > S2 -> F3 = F2 ; F3 = F4 )
	    )
	  ; F3 = F2
	  ),
	  logform_vars_to_pretty_symbols(F3, F4),
	  %% pretty symbols may let become quantified subformulas identical,
	  %% hence fast again:
	  simp_fol_fast(F4, F1),
	  simpinfo(Info, [F, F2, F1])
	).

simpinfo([]) :-
	!.
simpinfo(X) :-
	X \= [_|_],
	!,
	simpinfo([X]).
simpinfo(Formulas) :-
	simpinfo('Simp', Formulas).
simpinfo(Info, Formulas) :-
	findall(S, (member(F, Formulas), logform_size(F, S)), Sizes),
	info(10, 'Simp: formula sizes (~w): ~w', [Info, Sizes]).
