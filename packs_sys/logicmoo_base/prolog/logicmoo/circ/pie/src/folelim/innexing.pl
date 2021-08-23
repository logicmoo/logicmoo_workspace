:- module(innexing,
	  [qex/2,
	   quants_inward/2,
	   dnflike5/2,
	   cnflike5/2,
	   dnflike6/2,
	   cnflike6/2,
	   dnflike/2,
	   dnflike_l/2]).

:- use_module(folelim(logop_fol)).
:- use_module(folelim(simp_fol)).
:- use_module(folelim(prettysymbols_fol)).
:- use_module(folelim(auxiliary_fol)).
:- use_module(swilib(err)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Quine's Expansion
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% See https://arxiv.org/abs/1712.06868, Section "Quine’s Expansion"
%%%%
%%%% all x F[G] == (G & all x F[true]) v (~G & all x F[false])
%%%%  ex x F[G] == (G ; ex x F[false]) & (~G ; ex x F[true])
%%%%
%%%% TODO: more experiments, perhaps with further simplifications
%%%% such as moving quantifiers inward
%%%%
%%%% Works with logop_fol and tform, but not with Prolog variables as
%%%% representations of variables (due to implementation with findall).
%%%%

qex(F, F1) :-
	logform_set_x_counter(1),
	qex_simp_initial(F, F2),
	qexpand_fo_in_so(F2, F3),
	qex_simp_result(F3, F1).

qexpand_fo_in_so(ex2(X, F), ex2(X, F1)) :-
	\+ logform_has_second_order_quantifier(F),
	!,
	logform_process_subforms(F, qexpand, F3),
	qex_simp_result(F3, F1).
qexpand_fo_in_so(all2(X, F), all2(X, F1)) :-
	\+ logform_has_second_order_quantifier(F),
	!,
	logform_process_subforms(F, qexpand, F3),
	qex_simp_result(F3, F1).
qexpand_fo_in_so(F, F).

qexpand(all(X, F), F1) :-
	!,
	findall(Outs-MainF, qex_2(all(X, F), Outs, MainF), OM),
	qex_combine_all(OM, X, F2),
	qex_simp_combined(F2, F1).
qexpand(ex(X, F), F1) :-
	!,
	findall(Outs-MainF, qex_2(ex(X, F), Outs, MainF), OM),
	qex_combine_ex(OM, X, F2),
	qex_simp_combined(F2, F1).
qexpand(F, F).

qex_combine_all([OM], X, F) :-
	!,
	qex_combine_om_all(OM, X, F).
qex_combine_all([OM|OMs], X, (F1;F2)) :-
	!,
	qex_combine_om_all(OM, X, F1),
	qex_combine_all(OMs, X, F2).
qex_combine_all([], _, false).
	
qex_combine_ex([OM], X, F) :-
	!,
	qex_combine_om_ex(OM, X, F).
qex_combine_ex([OM|OMs], X, (F1,F2)) :-
	!,
	qex_combine_om_ex(OM, X, F1),
	qex_combine_ex(OMs, X, F2).
qex_combine_ex([], _, true).

qex_combine_om_all(OMs-F, X, F1) :-
	append(OMs, [all(X, F)], L),
	list_to_conjunction(L, F1).

qex_combine_om_ex(OMs-F, X, F1) :-
	append(OMs, [ex(X, F)], L),
	list_to_disjunction(L, F1).

qex_value(all, pos, true).
qex_value(all, neg, false).
qex_value(ex, pos, false).
qex_value(ex, neg, true).
qex_inner_redundant(all, false).
qex_inner_redundant(ex, true).

qex_2(F, [Out|Outs], MainF) :-
	logform_binder(F, X),
	( F = all(X1, F1) -> true, Quant = all, FRecur = all(X1, FRecur1)
	; F = ex(X1, F1) -> true, Quant = ex, FRecur = ex(X1, FRecur1)
	),
	logform_enum_subformulas_with_place(F1, SubF, _, _, NewF, Place),
	\+ ( logform_enum_free_constants(SubF, C),
	     memberq(C, X)
	   ),
	!,
	( Out = SubF,
	  qex_value(Quant, pos, Place)
	; logform_negate(SubF, Out),
	  qex_value(Quant, neg, Place)
	),
	FRecur1 = NewF,
	qex_simp_inner(FRecur, FRecur2),
	\+ qex_inner_redundant(Quant, FRecur2),
	qex_2(FRecur2, Outs, MainF).
qex_2(F, [], F).

qex_simp_inner(F, F1) :-
	simp_fol_fast(F, F2),
	logform_remove_void_quantifiers(F2, F3),
	logform_gather_quantifiers(F3, F1).

qex_simp_result(F, F1) :-
	simp_fol_fast(F, F2),
	logform_remove_void_quantifiers(F2, F1).

qex_simp_initial(F, F1) :-
	simp_fol_fast(F, F2),
	logform_remove_void_quantifiers(F2, F3),
	logform_gather_quantifiers(F3, F1).

qex_simp_combined(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_enum_free_constants(F, C) :-
	logform_enum_atoms(F, Atom, _, BoundSymbols),
	logatom_enum_constants(Atom, C),
	\+ logform_bindings_memberchk(C, BoundSymbols).

constant_occurs_free_in_logform(C, F) :-
	logform_enum_atoms(F, Atom, _, BoundSymbols),
	logatom_enum_constants(Atom, C1),
	C1 == C,
	\+ logform_bindings_memberchk(C, BoundSymbols),
	!.

predicate_occurs_free_in_logform(P, F) :-
	logform_enum_atoms(F, Atom, _, BoundSymbols),
	logatom_predicate(Atom, P1),
	P1 == P,
	\+ logform_bindings_memberchk(P, BoundSymbols),
	!.

logatom_enum_constants(_Pred/T, C) :-
	!,
	sub_term(C, T),
	C \== T,
	\+ compound(C).
logatom_enum_constants(LogAtom, C) :-
	sub_term(C, LogAtom),
	C \== LogAtom,
	\+ compound(C).

logatom_predicate(P/_, P) :-
	!.
logatom_predicate(A, P) :-
	functor(A, P, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Quantifiers Inward, without Expansion
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quants_inward(F, F1) :-
	logform_to_nnf(F, F2),
	qi1(F2, F1).

qi1(ex(X, F), F1) :-
	!,
	qi1(F, F2),
	( F2 = (G;H) ->
	  qi1(ex(X, G), G1),
	  qi1(ex(X, H), H1),
	  F1 = (G1;H1)
	; F2 = (G,H) ->
	  logupon_list(X, X0),
	  split_constants(X0, G, H, XGH, XG, XH),
	  ( XG = [] -> G1 = G
	  ; qi1(ex(XG, G), G1)
	  ),
	  ( XH = [] -> H1 = H
	  ; qi1(ex(XH, H), H1)
	  ),
	  ( XGH = [] -> F1 = (G1,H1)
	  ; F1 = ex(XGH, (G1,H1))
	  )
	; logupon_list(X, X0),
	  rm_void_constants(X0, F2, X1),
	  ( X1 = [] -> F1 = F2
	  ; F1 = ex(X1, F2)
	  )
	).
qi1(all(X, F), F1) :-
	!,
	qi1(F, F2),
	( F2 = (G,H) ->
	  qi1(all(X, G), G1),
	  qi1(all(X, H), H1),
	  F1 = (G1,H1)
	; F2 = (G;H) ->
	  logupon_list(X, X0),
	  split_constants(X0, G, H, XGH, XG, XH),
	  ( XG = [] -> G1 = G
	  ; qi1(all(XG, G), G1)
	  ),
	  ( XH = [] -> H1 = H
	  ; qi1(all(XH, H), H1)
	  ),
	  ( XGH = [] -> F1 = (G1;H1)
	  ; F1 = all(XGH, (G1;H1))
	  )
	; logupon_list(X, X0),
	  rm_void_constants(X0, F2, X1),
	  ( X1 = [] -> F1 = F2
	  ; F1 = all(X1, F2)
	  )
	).
qi1(ex2(X, F), F1) :-
	!,
	qi1(F, F2),
	( F2 = (G;H) ->
	  qi1(ex2(X, G), G1),
	  qi1(ex2(X, H), H1),
	  F1 = (G1;H1)
	; F2 = (G,H) ->
	  logupon_list(X, X0),
	  split_predicates(X0, G, H, XGH, XG, XH),
	  ( XG = [] -> G1 = G
	  ; qi1(ex2(XG, G), G1)
	  ),
	  ( XH = [] -> H1 = H
	  ; qi1(ex2(XH, H), H1)
	  ),
	  ( XGH = [] -> F1 = (G1,H1)
	  ; F1 = ex2(XGH, (G1,H1))
	  )
	; logupon_list(X, X0),
	  rm_void_predicates(X0, F2, X1),
	  ( X1 = [] -> F1 = F2
	  ; F1 = ex2(X1, F2)
	  )
	).
qi1(all2(X, F), F1) :-
	!,
	qi1(F, F2),
	( F2 = (G,H) ->
	  qi1(all2(X, G), G1),
	  qi1(all2(X, H), H1),
	  F1 = (G1,H1)
	; F2 = (G;H) ->
	  logupon_list(X, X0),
	  split_predicates(X0, G, H, XGH, XG, XH),
	  ( XG = [] -> G1 = G
	  ; qi1(all2(XG, G), G1)
	  ),
	  ( XH = [] -> H1 = H
	  ; qi1(all2(XH, H), H1)
	  ),
	  ( XGH = [] -> F1 = (G1;H1)
	  ; F1 = all2(XGH, (G1;H1))
	  )
	; logupon_list(X, X0),
	  rm_void_predicates(X0, F2, X1),
	  ( X1 = [] -> F1 = F2
	  ; F1 = all2(X1, F2)
	  )
	).
qi1((F,G), (F1,G1)) :-
	!,
	qi1(F, F1),
	qi1(G, G1).
qi1((F;G), (F1;G1)) :-
	!,
	qi1(F, F1),
	qi1(G, G1).
qi1(~F, ~F1) :-
	!,
	qi1(F, F1).
qi1(F, F).	  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% AUXILIARY
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rm_void_constants([X|Xs], F, Xs1) :-
	( constant_occurs_free_in_logform(X, F) ->
	  Xs1 = [X|Xs2]
	; Xs1 = Xs2
	),
	rm_void_constants(Xs, F, Xs2).
rm_void_constants([], _, []).

rm_void_predicates([X|Xs], F, Xs1) :-
	( predicate_occurs_free_in_logform(X, F) ->
	  Xs1 = [X|Xs2]
	; Xs1 = Xs2
	),
	rm_void_predicates(Xs, F, Xs2).
rm_void_predicates([], _, []).

split_constants(X, G, H, XGH, XG, XH) :-
	rm_void_constants(X, G, InG),
	rm_void_constants(X, H, InH),
	subtractq(InH, InG, XH),
	subtractq(InG, InH, XG),
	intersectionq(InH, InG, XGH).

split_predicates(X, G, H, XGH, XG, XH) :-
	rm_void_predicates(X, G, InG),
	rm_void_predicates(X, H, InH),
	subtractq(InH, InG, XH),
	subtractq(InG, InH, XG),
	intersectionq(InH, InG, XGH).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% DNFLIKE5
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dnflike5(F, F1) :-
	logform_process_subforms(F, all2_to_ex2, F2),
	logform_process_subforms(F2, to_dnflike5, F1).

to_dnflike5(ex2(X, F), ex2(X, F1)) :-
	\+ logform_has_second_order_quantifier(F),
	!,
	simp_fol_d5(F, F1).
to_dnflike5(F, F).

cnflike5(F, F1) :-
	logform_process_subforms(F, all2_to_ex2, F2),
	logform_process_subforms(F2, to_cnflike5, F1).

to_cnflike5(ex2(X, F), ex2(X, F1)) :-
	\+ logform_has_second_order_quantifier(F),
	!,
	simp_fol_c5(F, F1).
to_cnflike5(F, F).

dnflike6(F, F1) :-
	logform_process_subforms(F, all2_to_ex2, F2),
	logform_process_subforms(F2, to_dnflike6, F1).

to_dnflike6(ex2(X, F), ex2(X, F1)) :-
	\+ logform_has_second_order_quantifier(F),
	!,
	simp_fol_d6(F, F1).
to_dnflike6(F, F).

cnflike6(F, F1) :-
	logform_process_subforms(F, all2_to_ex2, F2),
	logform_process_subforms(F2, to_cnflike6, F1).

to_cnflike6(ex2(X, F), ex2(X, F1)) :-
	\+ logform_has_second_order_quantifier(F),
	!,
	simp_fol_c6(F, F1).
to_cnflike6(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% DNFLIKE
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dnflike(F, F1) :-
	logform_process_subforms(F, all2_to_ex2, F2),
	logform_process_subforms(F2, to_dnflike, F1).

all2_to_ex2(all2(X,F), ~ex2(X,~F)) :-
	!.
all2_to_ex2(F, F).

to_dnflike(ex2(X, F), ex2(X, F1)) :-
	\+ logform_has_second_order_quantifier(F),
	!,
	logform_process_subforms_with_polarity(F, to_dnflike_1, F1).
to_dnflike(F, F).

to_dnflike_1((F,(G;H)), p, ((F,G);(F,H))) :-
	!.
to_dnflike_1(((F;G),H), p, ((F,H);(G,H))) :-
	!.
to_dnflike_1((F;(G,H)), n, ((F;G),(F;H))) :-
	!.
to_dnflike_1(((F,G);H), n, ((F;H),(G;H))) :-
 	!.
to_dnflike_1(F, _, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% DNFLIKE_ORLINKED
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dnflike_l(F, F1) :-
	logform_process_subforms(F, all2_to_ex2, F2),
	logform_process_subforms(F2, to_dnflike_l, F1).

to_dnflike_l(ex2(X, F), ex2(X, F1)) :-
	\+ logform_has_second_order_quantifier(F),
	!,
	logupon_list(X, Ps),
	logform_process_subforms_with_polarity(F, to_dnflike_l_1(Ps), F1).
to_dnflike_l(F, F).

to_dnflike_l_1(Ps, (F,(G;H)), p, ((F,G);(F,H))) :-
	is_linked_pred(Ps, G, H),
	!.
to_dnflike_l_1(Ps, ((F;G),H), p, ((F,H);(G,H))) :-
	is_linked_pred(Ps, F, G),
	!.
to_dnflike_l_1(Ps, (F;(G,H)), n, ((F;G),(F;H))) :-
	is_linked_pred(Ps, G, H),
	!.
to_dnflike_l_1(Ps, ((F,G);H), n, ((F;H),(G;H))) :-
	is_linked_pred(Ps, F, G),
 	!.
to_dnflike_l_1(_, F, _, F).

is_linked_pred(Ps, F, G) :-
	logform_enum_atoms(F, Atom, Pol, Bound),
	logatom_predicate(Atom, P),
	memberq(P, Ps),
	\+ logform_bindings_memberchk(P, Bound),
	logform_enum_atoms(G, Atom1, Pol1, Bound1),
	logatom_predicate(Atom1, P1),
	P1 == P,
	pols_are_complementary(Pol, Pol1),
	\+ logform_bindings_memberchk(P, Bound1),
	!.

pols_are_complementary(p, n) :- !.
pols_are_complementary(n, p) :- !.
pols_are_complementary(pn, _) :- !.
pols_are_complementary(_, pn) :- !.
