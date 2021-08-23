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

:- module(elim_fol, [elim_fol/2,
		     elim_fol/3]).

:- use_module(nf(nf)).
:- use_module(swilib(err)).
:- use_module(swilib(info)).
:- use_module(swilib(pretty)).
:- use_module(swilib(fromonto)).
:- use_module(tform).
:- use_module(logop_fol).
:- use_module(auxiliary_fol).
:- use_module(formutils_fol).
:- use_module(simp_fol).
:- use_module(unskolemize).
:- use_module(prettysymbols_fol).
:- use_module(innexing).

dbg(X) :-
	get_conf(verbosity, N),
	(N >= 50 -> writex(X) ; true ).

writex(X) :-
	\+ \+ ( numbervars(X, 0, _), format('% ~q~n', [X])).


elim_fol(F, F1) :-
	elim_fol(F, [], F1).

elim_fol(F, Os, F1) :-
	preprocess_elim_fol(F, Os, F0),
	f_signature(F0, Preds, _),
	so_to_tnnfpl(F0, F3),
	elim_fol_1(F3, [reference_predicates=Preds|Os], F4),
	simp_elim_result(F4, F4a),
	logform_gather_quantifiers(F4a, F5),
	tnnfpl_to_so(F5, F6),
	% logform_vars_to_pretty_symbols(F6, F1),
	F1 = F6,
	( (F1 = true ; F1 = false) -> ! ; true ).

preprocess([Op|Ops], F, F1) :-
	preprocess_1(Op, F, F2),
	preprocess(Ops, F2, F1).
preprocess([], F, F).

preprocess_1(in, F, F1) :-
	!,
	info(20, 'Elim preprocessing: quantifiers inward'),
	quants_inward(F, F1).
preprocess_1(qex, F, F1) :-
	!,
	info(20, 'Elim preprocessing: Quine\'s expansion'),
	qex(F, F1).
preprocess_1(d, F, F1) :-
	!,
	info(20, 'Elim preprocessing: DNF-like'),
	dnflike(F, F1).
preprocess_1(d5, F, F1) :-
	!,
	info(20, 'Elim preprocessing: DNF-like (d5)'),
	dnflike5(F, F1).
preprocess_1(c5, F, F1) :-
	!,
	info(20, 'Elim preprocessing: CNF-like (c5)'),
	cnflike5(F, F1).
preprocess_1(d6, F, F1) :-
	!,
	info(20, 'Elim preprocessing: DNF-like (d6)'),
	dnflike6(F, F1).
preprocess_1(c6, F, F1) :-
	!,
	info(20, 'Elim preprocessing: CNF-like (c6)'),
	cnflike6(F, F1).
preprocess_1(dl, F, F1) :-
	!,
	info(20, 'Elim preprocessing: DNF-like for linked disjuncts'),
	dnflike_l(F, F1).
preprocess_1(Op, F, F) :-
	info(10, 'Elim: ignoring unimplemented preprocessing operation ~q', [Op]).

preprocess_elim_fol(F, Os, F1) :-
	( option(pre=Pre, Os) ->
	  preprocess(Pre, F, F1)
	; F1 = F
	).

%%%% 
%%%% for now: just walk through the formula, no simplifications at this level
%%%%
elim_fol_1(ex2(V, F), Os, F1) :-
	!,
% 	( memberchk(reference_predicates=RefPreds, Os) ->
% 	  true
% 	; RefPreds = []
% 	),
	% ensure_lscs(V, RefPreds, V1),
	%%% FEB 2016: FIRST PROCESS THE ARGUMENT!
	( logform_has_second_order_quantifier(F) ->
	  info(10, 'Elim: eliminating subform'),
	  elim_fol_1(F, Os, F2),
	  inprocess_subform(ex2(V, F2), Os, F3),
	  elim_fol_1(F3, Os, F1)
	; elim_fol_ex_0(V, F, Os, F1)
	).
elim_fol_1(all2(V, F), Os, F1) :-
	!,
	negate_nnf(F, NF),
	elim_fol_1(ex2(V, NF), Os, F2),
	negate_nnf(F2, F1).
elim_fol_1((F, G), Os, FG1) :-
	!,
	elim_fol_1(F, Os, F1),
	elim_fol_1(G, Os , G1),
	logform_conjoin(F1, G1, FG1).
elim_fol_1((F; G), Os, FG1) :-
	!,
	elim_fol_1(F, Os, F1),
	elim_fol_1(G, Os , G1),
	logform_disjoin(F1, G1, FG1).
elim_fol_1(all(X, F), Os, F1) :-
	!,
	elim_fol_1(F, Os, F2),
	allquant(X, F2, F1).
elim_fol_1(ex(X, F), Os, F1) :-
	!,
	elim_fol_1(F, Os, F2),
	exquant(X, F2, F1).
% elim_fol_1(rename(Spec, F), Os, F1) :-
% 	!,
% 	elim_fol_1(F, Os, F2),
% 	rename_fol(F2, Spec, F1).
elim_fol_1(F, _, F).

fol_simp(F, F1) :-
	fol_simp(F, [], F1).

fol_simp(F, _Os, F1) :-
	so_to_tnnfpl(F, F3),
	simp_result(F3, F4a),
	logform_gather_quantifiers(F4a, F5),
	tnnfpl_to_so(F5, F1).

fol_simp_v2(F, F1) :-
	fol_simp_v2(F, [], F1).

fol_simp_v2(F, _Os, F1) :-
	so_to_tnnfpl(F, F3),
	simp_result_v2(F3, F4a),
	logform_gather_quantifiers(F4a, F5),
	tnnfpl_to_so(F5, F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elim_fol_ex_0(V, F, Os, F1) :-
	ensure_lscs(V, F, V1),
	lscs_to_escs(V1, V2),
	elim_fol_ex(V2, F, Os, F1).

elim_fol_ex([], F, _, F) :-
	!.
elim_fol_ex(ESCS, F, Os, F1) :-
	( pick_item_to_eliminate(ESCS, F, Os, Item, ESCS1) ->
	  prepare_for_elim(Item, ESCS1, F, Os, F2),
	  elim_fol_2(Item, F2, Os, F3),
	  elim_fol_ex(ESCS1, F3, Os, F1)
	; F1 = F
	).

prepare_for_elim(Item, ESCS, F, _, F1) :-
	simp_before_separating(F, Item, ESCS, F1).

pick_item_to_eliminate([P|Ps], _, _, P, Ps).

elimitem_label(pred(_,L,_), L).
elimitem_label(predpos(_,L,_), L).
elimitem_label(predneg(_,L,_), L).

elimitem_arity(pred(_,_,N), N).
elimitem_arity(predpos(_,_,N), N).
elimitem_arity(predneg(_,_,N), N).

elimitem_predicate(pred(P,_,_), P).
elimitem_predicate(predpos(P,_,_), P).
elimitem_predicate(predneg(P,_,_), P).

elimitem_mode(pred(_,_,_), pred).
elimitem_mode(predpos(_,_,_), predpos).
elimitem_mode(predneg(_,_,_), predneg).

elim_fol_2(Item, F, Os, F1) :-
	strip_ex_prefix(F, F4, Es),
	elimitem_predicate(Item, P),
	expand_top_or_links(F4, P, F5),
	conj_groups(F5, P, CGs1),
	chk_conj_groups(CGs1, P, Es, CGs),
	map_process_cg(CGs, Item, Es, Os, F6),
	list_to_orseq(F6, F7),
	simp_combined_groups(F7, F1).

strip_ex_prefix(ex(V, F), F1, V1) :-
	!,
	logupon_list(V, V2),
	strip_ex_prefix(F, F1, V3),
	append(V2, V3, V1).
strip_ex_prefix(F, F, []).

% currently not resolved:
%
% simp_before_separating(((~A/t;q/t), (~A/t,q/t;A/t,p/t),
% (A/t;p/t)),pred(A,A,0),[],B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% CGs: conjunctive groups	
%%%% Es: outermost existential variables

%%%% 
%%%% -PXK, CX, D: components of the output formula 
%%%% -Q1: Universal prefix (list of Vars)
%%%% 
pullout_ack(F, P, Arity, Os, PXK, X, CX, D, Q1) :-
	pullout(F, [], P, Arity, [], F2, Q),
	%% inline_eq before skolemization, since may remove universal vars
	%% handle X like a free var by pullout: (?)
	inline_eq_pullout_result(F2, Q, F3, Q3),
	( option(skolemize(dependent),Os) ->
	  skolemize_dependent(Q3, F3, F5)	 
	; skolemize_top(Q3, F3, F5)
	),
	rm_universal_top(F5, F1, Q1),
	%% decompose F1:
	F1 = (all(X, (PX ; CX)), D),
	lit_complement_fol(PX, PXK).

simp_combined_groups(F, F1) :-
	align_unitpref(F, F2),
	prop_units(F2, F1).


% 	( elimitem_mode(Item, pred) ->
% 	  elimitem_predicate(Item, P),
% 	  split_id_p(F, P, F0),
% 	  ( F == F0 ->
% 	    true
% 	  ; info(20, 'Splitting upon instance applied')
% 	  )
% 	; F0 = F
% 	),


simp_before_separating(F, Item, ESCS, F1) :-
	simp_before_separating_1(F, Item, ESCS, F1).

simp_before_separating_1(F, Item, ESCS, F1) :-
	quants_gather(F, F2),
	align_unitpref(F2, F3),
	prop_units(F3, F4),
	inline_eq(F4, F5),
	topex_outwards(F5, F6),
	( F6 == F ->
	  F1 = F6
	; simp_before_separating_1(F6, Item, ESCS, F1)
	).

simp_elim_result(F, F1) :-
	simp_result(F, F1).

simp_ack_result(F, F1) :-
	simp_ack_result_1(F, F1).
	
simp_ack_result_1(F, F1) :-
	!,
	quants_gather(F, F2),
	align_unitpref(F2, F3),
	prop_units(F3, F3a),
	dist_eqcon(F3a, F4),
	inline_eq(F4, F5),
	( F5 == F ->
	  F1 = F5
	; simp_ack_result_1(F5, F1)
	).

simp_result_v2(F, F1) :-
	!,
	simp_ttlogform_fast(F, F0),
	quants_gather(F0, F2),
	inline_eq(F2, F2a),
	align_unitpref(F2a, F3),
	prop_units(F3, F3a),
	dist_eqcon(F3a, F4),
	inline_eq(F4, F5),
	( F5 == F ->
	  F1 = F5
	; simp_result_v2(F5, F1)
	).

simp_result(F, F1) :-
	!,
	simp_ttlogform_fast(F, F0),
	quants_gather(F0, F2),
	align_unitpref(F2, F3),
	prop_units(F3, F3a),
	dist_eqcon(F3a, F4),
	inline_eq(F4, F5),
	( F5 == F ->
	  F1 = F5
	; simp_result(F5, F1)
	).
	
%%%% 
%%%% Q: unskolemization before or after the final inline_eq?  In original DLS
%%%% it is performed before, but it seems that inline_eq possibly can effect
%%%% removal of universal variables (arguments of skolem functions)
%%%%

%%%% (all x (PX v CX)) & D
%%%% (all x (PX v (all AsP CX))) & D
%%%% (all x (PX v CX1)) & (all AsP D)
%%%% elim +q: (~q v C) & E[C]

%%%% ~r;q ~q;s
%%%%


%%%% 
%%%% nondeterministic
%%%% 
process_cg(cg(PP, PN, PZ, true), Item, Es, Os, F) :-
	!,
	elimitem_predicate(Item, P),
	elimitem_mode(Item, Mode),
	elimitem_label(Item, L),
	elimitem_arity(Item, Arity),

	( (\+ option(ack_nopos, Os) ; \+ option(ack_nothird, Os)) ->
	  process_cg_half(PP, PN, PZ, pos, Mode, P, L, Arity, Es, Os,
			  FP, SP, StatusP)
	; SP = 0, StatusP = unknown
	),

	( (\+ option(ack_noneg, Os) ; \+ option(ack_nothird, Os)) ->
	  process_cg_half(PN, PP, PZ, neg, Mode, P, L, Arity, Es, Os,
			  FN, SN, StatusN)
	; SN = 0, StatusN = unknown
	),
	info(10, 'Elim: sizes pos: ~w neg: ~w', [SP, SN]),
	dbg(fp(FP)),
	dbg(fn(FN)),
	( option(ack_noneg, Os) ->
	  ( StatusP \= success ->
	    info(10, 'Elim: unskolemization failed ~q', [StatusP]),
	    fail
	  ; nonvar(FP), F = FP
	  )
	; option(ack_nopos, Os) ->
	  ( StatusN \= success ->
	    info(10, 'Elim: unskolemization failed ~q', [StatusN]),
	    fail
	  ; nonvar(FN), F = FN
	  )
	; ( StatusP = success ->
	    ( StatusN = success ->
	      ( SN < SP ->
		(nonvar(FN), F = FN ; (\+ variant(FP,FN) -> F = FP))
	      ; (nonvar(FP), F = FP ; (\+ variant(FP,FN) -> F = FN))
	      )
	    ; nonvar(FP),
	      F = FP
	    )
	  ; StatusN = success ->
	    nonvar(FN),
	    F = FN
	  ; info(10, 'Elim: unskolemization failed for both possibilities:~n ~q~n ~q',
		[StatusN, StatusP]),
	    fail
	  )
 	).
process_cg(CG, _, _, _) :-
	err('Failed to process ~q', [CG]).

map_process_cg([X|Xs], Y1, Y2, Y3, [X1|Xs1]) :-
	process_cg(X, Y1, Y2, Y3, X1),

	map_process_cg(Xs, Y1, Y2, Y3, Xs1).
map_process_cg([], _, _, _, []).


process_cg_half(PP, PN, PZ, Sign, Mode, P, L, Arity, Es, Os, F, Size, Status) :-
	tnnfpl_copy_bound_vars(PP, PP1),
	tnnfpl_copy_bound_vars(PN, PN1),
	tnnfpl_copy_bound_vars(PZ, PZ1),
	( Sign = pos -> P1 = +(P) ; P1 = -(P) ),
	pullout_ack(PP1, P1, Arity, Os, PX, X, CX, D, As),
	allquant(As, CX, CX1),
	apply_ack(PN1, PX, X, CX1, C1),
	info(100, 'Elim: Ackermann\'s lemma: ~q', [apply_ack(PN1, PX, X, CX1, C1)]),
	copy_given_vars(As, D, As0, D0),
	allquant(As0, D0, D1),
	logform_conjoin(PZ1, D1, D2), 
	logform_conjoin(C1, D2, D3),
	exquant(Es, D3, F1),
	( Mode = pred ->
	  F1a = F1
	; ((Sign = pos, Mode = predneg) ; (Sign = neg, Mode = predpos)) ->
	  px_negate(PX, PX1),
	  copy_given_vars([P], PX1, [L], PX2),
	  %% Replace P with the label L in the output, assuming that it is
	  %% not used for elimination again (ensured by forbidding nesting
	  %% of polarity sensitive quantification like ex2([+q,-q], ...)
	  F1a = (F1, all(X, (PX2 ; CX1)))
	; true
	),
	( nonvar(F1a) ->
	  simp_ack_result(F1a, F2),
	  ( option(no_unsk, Os) ->
	    F = F2,
	    Status = success
	  ; tnnfpl_unskolemize(F2, Os, F, Status)
	  ),
	  nnf_size(F, Size)
	; Size = 0,
	  Status = unknown
	).


apply_ack(F, PX, X, CX, F1) :-
	is_lit_instance_of(F, PX, T),
	!,
	tnnfpl_copy_bound_vars(CX, CX1),
	copy_given_vars(X, CX1, XT, CX2),
	bind_tuple(T, XT),
	F1 = CX2.
apply_ack((F;G), PX, X, CX, FG) :-
	!,
	apply_ack(F, PX, X, CX, F1),
	apply_ack(G, PX, X, CX, G1),
	logform_disjoin(F1, G1, FG).
apply_ack((F,G), PX, X, CX, FG) :-
	!,
	apply_ack(F, PX, X, CX, F1),
	apply_ack(G, PX, X, CX, G1),
	logform_conjoin(F1, G1, FG).
apply_ack(ex(V,F), PX, X, CX, F1) :-
	!,
	apply_ack(F, PX, X, CX, F2),
	exquant(V, F2, F1).
apply_ack(all(V,F), PX, X, CX, F1) :-
	!,
	apply_ack(F, PX, X, CX, F2),
	allquant(V, F2, F1).
apply_ack(L, _, _, _, L).

px_negate(~PX, PX) :-
	!.
px_negate(PX, ~PX).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Preparation: Multiplication of Linked Disjunctions
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
f+p & g-p & (h+p v j-p)

f+p & g-p & h+p
f+p & g-p & j-p

f+p & g-p & ((h+p & k-p) v j-p)
f+p & g-p & h+p & k-p
f+p & g-p & ((h+p & (k+p v r-p)) v j-p)
*/

%%%% 
%%%% 
%%%% 
expand_top_or_links(ex(X, F), P, ex(X, G)) :-
	!,
	expand_top_or_links(F, P, G).
expand_top_or_links(F, P, G) :-
	expand_top_or_links_1(F, P, G).

expand_top_or_links_1((F;G), P, (F1;G1)) :-
	!,
	expand_top_or_links_1(F, P, F1),
	expand_top_or_links_1(G, P, G1).
expand_top_or_links_1((F,G), P, FG1) :-
	!,
	expand_top_or_links_1(F, P, F2),
	expand_top_or_links_1(G, P, G2),
	( multiply_step((F2,G2), P, FG1) ->
	  true
	; FG1 = (F2,G2)
	).
expand_top_or_links_1(F, _, F).

multiply_step((A,(F;G)), P, ((A,F);(A1,G))) :-
	is_linked(P, F, G),
	!,
	tnnfpl_copy_bound_vars(A, A1).
multiply_step(((F;G),A), P, ((A,F);(A1,G))) :-
	is_linked(P, F, G),
	!,
	tnnfpl_copy_bound_vars(A, A1).

is_linked(P, F, G) :-
	tnnfpl_contains_predicate(F, P, p),
	tnnfpl_contains_predicate(G, P, n),
	!.
is_linked(P, F, G) :-
	tnnfpl_contains_predicate(F, P, n),
	tnnfpl_contains_predicate(G, P, p).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Conj Groups, i.e. Tuples of:
%%%% 
%%%% - Conjunct with positive pred
%%%% - Conjunct with negative pred
%%%% - Conjunct without pred
%%%% - Conjunct with pred in both polarities
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chk_conj_groups(Gs, P, Es, Gs1) :-
	chk_conj_groups(Gs, P, Es, [], Gs1).

chk_conj_groups([G|Gs], P, Es, Gs0, Gs1) :-
	cg_is_false(G),
	!,
	chk_conj_groups(Gs, P, Es, Gs0, Gs1).
chk_conj_groups([G|Gs], P, Es, Gs0, _) :-
	cg_is_unsolvable(G),
	!,
	info(10, 'Elim: separation phase fails in the following conjunction:'),
	info_ppcg(10, [G], P-Es),
	%% asserta(cg(G)),
	append(Gs, Gs0, Gs1),
	( Gs1 \== [] ->
	  info(10, 'Other conjunctions:'),
	  info_ppcg(10, Gs1, P)
	; true
	),
	fail.
chk_conj_groups([G|Gs], P, Es, Gs0, [G|Gs1]) :-
	chk_conj_groups(Gs, P, Es, [G|Gs0], Gs1).
chk_conj_groups([], _, _, _, []).

cg_is_false(cg(false,_,_,_)).
cg_is_false(cg(_,false,_,_)).
cg_is_false(cg(_,_,false,_)).
cg_is_false(cg(_,_,_,false)).

cg_is_unsolvable(cg(_,_,_,F)) :-
	F \== true.

conj_groups(F, P, Gs) :-
	conj_groups_1(F, Gs1),
	map_mk_conj_group(Gs1, P, Gs).

conj_groups_1((F;G), Gs) :-
	!,
	conj_groups_1(F, F1),
	conj_groups_1(G, G1),
	append(F1, G1, Gs).
conj_groups_1(F, [F]).

mk_conj_group(F, P, cg(PP,PN,PZ,PB)) :-
	split_conjuncts_with_pred(F, P, PP1, PN1, PZ1, PB1),
 	simp_ttlogform_fast(PP1, PP),
 	simp_ttlogform_fast(PN1, PN),
 	simp_ttlogform_fast(PZ1, PZ),
 	simp_ttlogform_fast(PB1, PB).

map_mk_conj_group([X|Xs], Y1, [X1|Xs1]) :-
	mk_conj_group(X, Y1, X1),
	map_mk_conj_group(Xs, Y1, Xs1).
map_mk_conj_group([], _, []).

split_allquant(all(X, (F, G)), all(X, F), all(X, G)) :-
	!.
split_allquant(all(X, all(Y, F)), all(X, F1), all(X, F2)) :-
	split_allquant(all(Y, F), F1, F2).

split_conjuncts_with_pred((F,G), P, PP, PN, PZ, PB) :-
	!,
	split_conjuncts_with_pred(F, P, FPP, FPN, FPZ, FPB),
	split_conjuncts_with_pred(G, P, GPP, GPN, GPZ, GPB),
	logform_conjoin(FPP, GPP, PP),
	logform_conjoin(FPN, GPN, PN),
	logform_conjoin(FPZ, GPZ, PZ),
	logform_conjoin(FPB, GPB, PB).
split_conjuncts_with_pred(F, P, PP, PN, PZ, PB) :-
	split_allquant(F, F1, G1),
	!,
	tnnfpl_copy_bound_vars(G1, G2),
	split_conjuncts_with_pred((F1, G2), P, PP, PN, PZ, PB).
split_conjuncts_with_pred(F, P, PP, PN, PZ, PB) :-
	( tnnfpl_contains_predicate(F, P) ->
	  ( tnnfpl_contains_predicate(F, P, p) ->
	    ( tnnfpl_contains_predicate(F, P, n) ->
	      split_id_p(F, P, F1),
	      ( F1 =@= F ->
		%% use =@= instead of == here as split_id_p may rename
		%% quantified variables
		PP = true, PN = true , PZ = true, PB = F
	      ; info(20, 'Elim: splitting upon instance applied'),
		split_conjuncts_with_pred(F1, P, PP, PN, PZ, PB)
	      )
	    ; PP = F, PN = true , PZ = true, PB = true
	    )
	  ; tnnfpl_contains_predicate(F, P, n) ->
	    PP = true, PN = F , PZ = true, PB = true
          )
	; PP = true, PN = true , PZ = F, PB = true
	).

info_ppcg(Verbosity, X, Y) :-
	get_info_verbosity(Verbosity1),
	( Verbosity1 >= Verbosity ->
	  onto_stream(ppcg(X,Y), user_error)
	; true
	).

% ppcg([cg(PP,PN,PZ,PB)|Tuples], P) :-
% 	fail,
% 	!,
% 	format('--- cg ---~n'),
% 	map_logform_gather_quantifiers([PP,PN,PZ,PB], [PP1,PN1,PZ1,PB1]),
%  	\+ \+ ( numbervars([P,PP1,PN1,PZ1,PB1], 0, _),
%  		format('p: ~q~n+: ~q~n-: ~q~n0: ~q~nx: ~q~n',
%  		       [P,PP1,PN1,PZ1,PB1])),
% 	ppcg(Tuples, P).
ppcg([cg(PP,PN,PZ,PB)|Tuples], P) :-
	format('--- cg ---~n'),
	map_logform_gather_quantifiers([PP,PN,PZ,PB], [PP1,PN1,PZ1,PB1]),
	map_tnnfpl_to_so([PP1,PN1,PZ1,PB1], [PP2,PN2,PZ2,PB2]),
	logform_vars_to_pretty_symbols(PP2, PP3),
	logform_vars_to_pretty_symbols(PN2, PN3),
	logform_vars_to_pretty_symbols(PZ2, PZ3),
	logform_vars_to_pretty_symbols(PB2, PB3),
	format('+: ~q~n-: ~q~n0: ~q~nx: ~q~n',
	       [PP3,PN3,PZ3,PB3]),
	ppcg(Tuples, P).
ppcg([], _).

map_logform_gather_quantifiers([X|Xs], [X1|Xs1]) :-
	logform_gather_quantifiers(X, X1),
	map_logform_gather_quantifiers(Xs, Xs1).
map_logform_gather_quantifiers([], []).

map_tnnfpl_to_so([X|Xs], [X1|Xs1]) :-
	tnnfpl_to_so(X, X1),
	map_tnnfpl_to_so(Xs, Xs1).
map_tnnfpl_to_so([], []).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skolemize_top(Prefix, F, F1) :-
	add_prefix(Prefix, F, F2),
	skolemize_top_1(F2, [], Prefix, F1).
	
skolemize_top_1(ex(V, F), Us, Prefix, F1) :-
	!,
	logupon_list(V, V1),
	%% free variables from outside might be transported in the dependency
	%% info
	( V1 = [V0],
	  member(ex(X, Dep), Prefix), X == V0 ->
	  unionq(Us, Dep, Us1)
	; Us1 = Us
	),
	bind_to_skolem_terms(V1, Us1),
	skolemize_top_1(F, Us, Prefix, F1).
skolemize_top_1(all(V, F), Us, Prefix, all(V, F1)) :-
	!,
	logupon_list(V, V1),
	append_newq(Us, V1, V2),
	skolemize_top_1(F, V2, Prefix, F1).
skolemize_top_1(F, _, _, F).

skolemize_dependent([all(X)|P], F, all([X], F1)) :-
	!,
	skolemize_dependent(P, F, F1).
skolemize_dependent([ex(X,D)|P], F, F1) :-
	!,
	sort(D, D1),
	bind_to_skolem_term(X, D1),
	skolemize_dependent(P, F, F1).
skolemize_dependent([], F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Prefix: List of terms all(Var), ex(Var, DominatingVars)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Remove universal vars that are not in given list of vars
%%%% Keep existential vars
%%%%
prefix_restrict_universal_vars([all(X)|Ps], Vs, [all(X)|Ps1]) :-
	memberq(X, Vs),
	!,
	prefix_restrict_universal_vars(Ps, Vs, Ps1).
prefix_restrict_universal_vars([all(_)|Ps], Vs, Ps1) :-
	!,
	prefix_restrict_universal_vars(Ps, Vs, Ps1).
prefix_restrict_universal_vars([ex(X,U)|Ps], Vs, [ex(X,U)|Ps1]) :-
	!,
	prefix_restrict_universal_vars(Ps, Vs, Ps1).
prefix_restrict_universal_vars([], _, []).

prefix_restrict_ex_dependencies([all(X)|Ps], Vs, [all(X)|Ps1]) :-
	!,
	prefix_restrict_ex_dependencies(Ps, Vs, Ps1).
prefix_restrict_ex_dependencies([ex(X,U)|Ps], Vs, [ex(X,U1)|Ps1]) :-
	!,
	intersectionq(U, Vs, U1),
	prefix_restrict_ex_dependencies(Ps, Vs, Ps1).
prefix_restrict_ex_dependencies([], _, []).

prefix_universal_tail(P, P1) :-
	reverse(P, P2),
	put_1(P2, P3),
	reverse(P3, P1).

prefix_vars([all(X)|Ps], [X|Vs]) :-
	!,
	prefix_vars(Ps, Vs).
prefix_vars([ex(X,_)|Ps], [X|Vs]) :-
	!,
	prefix_vars(Ps, Vs).
prefix_vars([], []).

put_1([all(X)|Ps], [X|Ps1]) :-
	!,
	put_1(Ps, Ps1).
put_1(_, []).

prefix_remove_duplicates(P, P1) :-
	reverse(P, P2),
	prefix_rr(P2, P3),
	reverse(P3, P1).

prefix_rr([X|Xs], Xs1) :-
	memberq(X, Xs),
	!,
	prefix_rr(Xs, Xs1).
prefix_rr([X|Xs], [X|Xs1]) :-
	prefix_rr(Xs, Xs1).
prefix_rr([], []).

add_prefix([], F, F).
add_prefix([all(X)|Ps], F, all([X], F1)) :-
	add_prefix(Ps, F, F1).
add_prefix([ex(X,_)|Ps], F, ex([X], F1)) :-
	add_prefix(Ps, F, F1).

rm_universal_top(all(V,F), F1, Vs) :-
	!,
	logupon_list(V, Vs1),
	rm_universal_top(F, F1, Vs2),
	append(Vs1, Vs2, Vs).
rm_universal_top(F, F, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

option(X, Os) :-
	memberchk(X, Os).

% inline_eq_pullout_result(F, Q, F, Q) :-
%   	!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Special version of inline_eq for pullout results that preserves the shape
%%%% of these. This possibly can be made stronger by allowing to bind
%%%% universal variables in the prefix Q that are there not in front of an
%%%% existential quantifier, and are not shared by all(X, (PX ; C)) and D.  The
%%%% output prefix Q1 then has to be adapted accordingly.
%%%%
inline_eq_pullout_result((all(X, (PX ; C)), D),
			 Q,
			 (all(X, (PX ; C1)), D1),
			 Q1) :-
	prefix_vars(Q, E),
	unionq(X, E, EC),
 	inline_eq(C, [], EC, EC, C1),
 	inline_eq(D, [], E, E, D1),
	Q1 = Q.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rename_fol(F, Spec, F1) :-
% 	replace_atoms_fol(F, t_rename_atom-Spec, F1).
% 
% t_rename_atom([], A, A).
% t_rename_atom([From-To|Spec], P/A, P1/A) :-
% 	get_group(P, G),
% 	( G = From ->
% 	  set_group(P, To, P2)
% 	; P2 = P
% 	),
% 	t_rename_atom(Spec, P2/A, P1/A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bind_to_skolem_terms([V|Vs], Args) :-
	bind_to_skolem_term(V, Args),
	bind_to_skolem_terms(Vs, Args).
bind_to_skolem_terms([], _).

bind_to_skolem_term(Var, Args) :-
	logform_gen_skolem_functor(Functor),
	Var =.. [Functor|Args].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tuple_functor(t).

bind_tuple(Value, Vars) :-
	tuple_functor(T),
	\+ var(Value),
	Value =.. [T|Values],
	!,
	( Vars = Values ->
	  true
	; err('Bind tuple failed: ~q ~q', [Value, Vars])
	).
bind_tuple(Value, [Value]) :-
	!.
bind_tuple(Value, Vars) :-
	err('Bind tuple failed: ~q ~q', [Value, Vars]).

is_lit_instance_of(~(P1/T), ~(P/X), T) :-
	subsumes_chk(X, T),
	P1 == P.
is_lit_instance_of(P1/T, P/X, T) :-
	subsumes_chk(X, T),
	P1 == P.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lit_complement_fol(~(L), L) :-
	!.
lit_complement_fol(L, ~(L)).

lit_args(~(_/Args), Args) :-
	!.
lit_args(_/Args, Args).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Scope Specifier - Deprecated (?)
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% scs: canonical form, used internally:
%%%%  all-LSCS | LSCS
%%%%
%%%% lscs: list component of scs, sorted, types of members:
%%%%    predpos(P)
%%%%    predneg(P)
%%%%
%%%% uscs: user form
%%%%
%%%% escs: form used by the existential elimination predicated
%%%%

ensure_lscs(all-_, _, _) :-
	err('Invalid use of subtraction scope specifier.').
ensure_lscs(X, _, Y) :-
	is_lscs(X),
	!,
	sort(X, Y).
ensure_lscs(X, RefPreds, Y) :-
	uscs_to_lscs(X, RefPreds, Y).

%%%% 
%%%% No test for being sorted
%%%%
is_lscs(X) :-	var(X),	!, fail.
is_lscs([X|_]) :- var(X), !, fail.
is_lscs([predpos(_,_,_)|X]) :- !, is_lscs(X).  
is_lscs([predneg(_,_,_)|X]) :- !, is_lscs(X).  
is_lscs([]).

uscs_to_lscs(X, S) :-
	uscs_to_lscs(X, [], S).

%%%% 
%%%% RefForm is just used to detect arity.
%%%% 
uscs_to_lscs(X, RefForm, S) :-
	var(X),
	!,
	uscs_to_lscs([X], RefForm, S).
uscs_to_lscs([], _, []) :-
	!.
uscs_to_lscs(X, RefForm, S) :-
	X = [_|_],
	!,
	map_uscs_to_lscs_1(X, RefForm, S1),
	sort(S1, S).
uscs_to_lscs(X, RefForm, S) :-
	uscs_to_lscs([X], RefForm, S).

uscs_ctx_lookup_arity(P, RefForm, N) :-
	logform_enum_atoms(RefForm, P1/T),
	P == P1,
	!,
	functor(T, _, N).
%%%% NEW Feb 2017: fail if no atom with P is found in RefForm
% uscs_ctx_lookup_arity(P, F, _) :-
% 	err('Failed to detect predicate arity: ~q in ~q', [P, F]).	

map_uscs_to_lscs_1([X|Xs], RefForm, Xs1) :-
	( var(X) ->
	  P = X,
	  ( uscs_ctx_lookup_arity(P, RefForm, N) ->
	    Xs1 = [predneg(P,P,N), predpos(P,P,N)|Xs2]
	  ; %% if P does not occur in RefForm
	    Xs1 = Xs2
	  )
	; X = +(P/N) ->
	  Xs1 = [predpos(P,P,N)|Xs2]
	; X = -(P/N) ->
	  Xs1 = [predneg(P,P,N)|Xs2]
	; X = P/N ->
	  Xs1 = [predneg(P,P,N), predpos(P,P,N)|Xs2]
	; X = +(P), atom(P) ->
	  uscs_ctx_lookup_arity(P, RefForm, N),
	  Xs1 = [predpos(P,P,N)|Xs2]
	; X = -(P), atom(P) ->
	  uscs_ctx_lookup_arity(P, RefForm, N),
	  Xs1 = [predneg(P,P,N)|Xs2]
	; atom(X) ->
	  P = X,
	  uscs_ctx_lookup_arity(P, RefForm, N),
	  Xs1 = [predneg(P,P,N), predpos(P,P,N)|Xs2]
	%% allow also results from nf:rename_vars
	; X = pred(P,L,N) ->
	  Xs1 = [predneg(P,L,N), predpos(P,L,N)|Xs2]
	; X = predpos(P,L,N) ->
	  Xs1 = [predpos(P,L,N)|Xs2]
	; X = predneg(P,L,N) ->
	  Xs1 = [predneg(P,L,N)|Xs2]
	; err('Bad item in scope specifier: ~q', [X])
	),
	map_uscs_to_lscs_1(Xs, RefForm, Xs2).
map_uscs_to_lscs_1([], _, []).

%%%% 
%%%% 
lscs_to_escs(LSCS, ESCS) :-
	lscs_gather_posneg(LSCS, ESCS).

%%%% 
%%%% 
escs_to_lscs(ESCS, LSCS) :-
	escs_expand_1(ESCS, LSCS1),
	sort(LSCS1, LSCS).

escs_expand_1([pred(P,L,N)|SCS], [predpos(P,L,N), predneg(P,L,N)|SCS1]) :-
	!,
	escs_expand_1(SCS, SCS1).
escs_expand_1([X|SCS], [X|SCS1]) :-
	escs_expand_1(SCS, SCS1).
escs_expand_1([], []).
	
lscs_gather_posneg(SCS, SCS1) :-
	map_lscs_comp_1(SCS, SCS2),
	sort(SCS2, SCS3),
	map_lscs_comp_2(SCS3, SCS4),
	sort(SCS4, SCS1).

map_lscs_comp_1([X|Xs], [P-X|Xs1]) :-
	arg(1, X, P),
	map_lscs_comp_1(Xs, Xs1).
map_lscs_comp_1([], []).

map_lscs_comp_2([P1-predneg(_,B,N),P2-predpos(_,B,N)|Xs], [pred(P1,B,N)|Xs1]) :-
	P1 == P2,
	!,
	map_lscs_comp_2(Xs, Xs1).
map_lscs_comp_2([_-X|Xs], [X|Xs1]) :-
	map_lscs_comp_2(Xs, Xs1).
map_lscs_comp_2([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Experimental
%%%% 
%%%% Rewrite subformulas F[A] with complementary occurrences of atoms A
%%%% into (A ; F[false]), (~A ; F[true]), with simplifications,
%%%% only if A has no bound symbols. Do this wrt subformulas, i.e.
%%%% the symbols in A may be bound by a surrounding quantifier.
%%%% 
%%%% This is an incomplete distribution technique.
%%%%
%%%% Should work for logform as well ass ttform.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% The should work ttform (maybe also for logform)
%%%%

ttlogform_is_atom(F) :-	logform_is_atom(F).

ttlogform_atom_functor(P/_, X) :- !, X == P.
ttlogform_atom_functor(A, F) :- functor(A, F, _).

split_id_p(F, P, F1) :-
	logform_process_subforms(F, split_id_p_1(P), F1).

split_id_p_1(P, F, F1) :-
	logform_process_subforms_with_polarity_and_bindings(F, split_id_p_2(P,Atom,Place,Found), F2),
	Found == pol(found, found),
	!,
	free_variables(F2, Vs),
	abs_delete_once(Vs, Place, Vs1),
	copy_term(Vs1-Place-F2, Vs1-false-F3),
	tnnfpl_copy_bound_vars(F3, F3a),
	Place = true,
	simp_ttlogform_fast(F2, FT1),
	simp_ttlogform_fast(F3a, FF1),
	F4 = ((Atom; FF1) , (~Atom; FT1)),
	simp_ttlogform_fast(F4, F1).
split_id_p_1(_, F, F).
	
split_id_p_2(P, Atom, Place, pol(PP,PN), F, Pol, B, F1) :-
	ttlogform_is_atom(F),
	( var(Atom) ->
	  ttlogform_atom_functor(F, P),
	  \+ (member(X, B), contains_var(X, F)),
	  Atom = F
	; Atom == F
	),
	!,
	( Pol = p -> PP = found
	; Pol = n -> PN = found
	),
	F1 = Place.
split_id_p_2(_, _, _, _, F, _, _, F).

	
	
abs_delete_once([X|Xs], Y, Xs) :-
	X == Y,
	!.
abs_delete_once([X|Xs], Y, [X|Ys]) :-
	abs_delete_once(Xs, Y, Ys).
abs_delete_once([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inprocess_subform(F, Os, F1) :-
	( memberchk(inp=Ops, Os), Ops \= [] ->
	  info(20, 'Elim: inprocessing subformula'),
	  %% Instantiate bound vars (the clean property is not essential here)
	  logform_clean_vars_to_symbols(F, F3),
	  %% The predicates to eliminate and possible vars from the context
	  %% are now still the original prolog vars. Instantiate them in a copy.
	  term_variables(F3, FreeVars),
	  copy_term(F3-FreeVars, F4-FreeVars1),
	  instantiate(FreeVars1, FreeVars, UndoMap),
	  preprocess(Ops, F4, F5),
	  %% restore prolog vars representing predicates to eliminate
	  subst_atoms(F5, UndoMap, F6),
	  %% convert bound vars to clean prolog vars
	  logform_clean_vars_to_prolog_vars(F6, F1)
	; F1 = F
	).

instantiate(Vs, UndoVs, UndoMap) :-
	instantiate_1(Vs, UndoVs, 1, UndoMap).

instantiate_1([A|As], [V|Vs], N, [A-V|Map]) :-
	concat_atom(['$inst_', N], A),
	N1 is N+1,
	instantiate_1(As, Vs, N1, Map).
instantiate_1([], _, _, []).

