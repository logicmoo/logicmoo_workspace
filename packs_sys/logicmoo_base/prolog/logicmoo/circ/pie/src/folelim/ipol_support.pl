%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2018 Christoph Wernhard
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

:- module(ipol_support,
	  [ip_simp_combined/5,
	   ip_prep_singleside/5,
	   ip_simp_singleside/4,
	   quantify_unshared_constants/2,
	   bodylist_to_bodyform/2,
	   bodylist_to_headform/2,
	   bodyform_to_bodylist/2,
	   headform_to_bodylist/2,
	   verify_interpolant/3]).

:- use_module(swilib(err)).
:- use_module(swilib(options)).
:- use_module(external_prover9).
:- use_module(prep_resol).
:- use_module(preprocexp, [m_red_condense_limited/3]).

%% *** ADD EQUALITY

ip_simp_combined(MA, MB, Options, MA1, MB1) :-
	%%
	%% apply no simplifications that alter clauses here, since the input
	%% clauses are required later to process the proof. Ordering of
	%% literals may be altered.
	%%
	( from_options(ip_simp_combined=false, Options, true) ->
	  MA1 = MA,
	  MB1 = MB
	; append(MA, MB, M1),
	  info(20, 'Simplification: purity'),
	  fol_red_pure(M1, M2),
	  info(20, 'Simplification: subsumption'),
	  threshold(inferences_fol_red_subs, Limit),
	  m_red_subs_limited(M2, Limit, M3),
	  split_combined(M3, MB, MA1, MB1)
	).

split_combined([C|Cs], MB, MA1, MB1) :-
	( member(C1, MB),
	  C == C1 ->
	  MA1 = MA2,
	  MB1 = [C|MB2]
	; MA1 = [C|MA2],
	  MB1 = MB2
	),
	split_combined(Cs, MB, MA2, MB2).
split_combined([], _, [], []).

ip_prep_singleside(M, Side, OtherSideM, Options, M1) :-
	m_features(M, Features),
	m_features(OtherSideM, OtherSideFeatures),
	( memberchk(equality, OtherSideFeatures) ->
	  ( select(noequality, Features, Features1) ->
	    true
	  ; Features1 = Features
	  ),
	  Features2 = [equality|Features1]
	; Features2 = Features
	),
	format(atom(Info), 'Side ~w', [Side]),
	m_info_features(20, Features2, Info),
	m_set_equality(M, Features2, Options, M1, _).
	
ip_simp_singleside(M, SharedPs, Options, M1) :-
	%%	
	%% apply no simplifications that rely on something *not* present, like
	%% general purity here
	%%
	( from_options(ip_simp_sides=false, Options, true) ->
	  M3 = M
	; info(20, 'Simplification: purity'),
	  m_red_purity(M, SharedPs, M2),
	  info(20, 'Simplification: cheap elimination'),
	  m_red_elim_cheap(M2, SharedPs, M3)
	),
	( from_options(ip_simp_condense=false, Options, true) ->
	  M1 = M3
	; fol_sort_lits_by_pred_occurrences(M3, M4),
	  threshold(inferences_m_red_condense, Limit),
	  info(20, 'Simplification: condensation, limited to ~w', [Limit]),
	  m_red_condense_limited(M4, Limit, M1)
	).

%%%% 
%%%% probably not really needed anymore in presence of lifting
%%%% that considers this too	  
%%%% 
quantify_unshared_constants((F -> G), (F1 -> G1)) :-
	f_signature(F, _, FF),
	f_signature(G, _, FG),
	findall(XF, member(XF/0, FF), XFs1),
	sort(XFs1, XFs2),
	findall(XG, member(XG/0, FG), XGs1),
	sort(XGs1, XGs2),
	ord_subtract(XFs2, XGs2, ExF),
	ord_subtract(XGs2, XFs2, AllG),
	( ExF = [] -> F1 = F ; F1 = ex(ExF, F) ),
	( AllG = [] -> G1 = G ; G1 = all(AllG, G) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% These decompose only on a shallow level to ensure applicability on large
%%%% input.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bodylist_to_bodyform([], true).
bodylist_to_bodyform([F], F) :-
	!.
bodylist_to_bodyform([F|Fs], (F,F1)) :-
	bodylist_to_bodyform(Fs, F1).

bodyform_to_bodylist((F, F1), [F|Fs]) :-
	!,
	bodyform_to_bodylist(F1, Fs).
bodyform_to_bodylist(true, []) :-
	!.
bodyform_to_bodylist(~((F;F1)), Fs) :-
	!,
	bodyform_to_bodylist((~F,~F1), Fs).
bodyform_to_bodylist(~true, [false]) :-
	!.
bodyform_to_bodylist(~false, []) :-
	!.
bodyform_to_bodylist(~(~F), [F]) :-
	!.
bodyform_to_bodylist(F, [F]).


headform_to_bodylist(F, Fs) :-
	!,
	dtl_1(F, Fs).

dtl_1((F; F1), [NF1|Fs]) :-
	!,
	negate(F, NF1),
	dtl_1(F1, Fs).
dtl_1(false, []) :-
	!.
dtl_1(~((F, F1)), Fs) :-
	!,
	dtl_1((~F;~F1), Fs).
dtl_1(~(true), [true]) :-
	!.
dtl_1(~(false), []) :-
	!.
dtl_1(F, [NF]) :-
	negate(F, NF).

bodylist_to_headform([], false).
bodylist_to_headform([F], F1) :-
	!,
	negate(F, F1).
bodylist_to_headform([F|Fs], (F1;F2)) :-
	negate(F, F1),
	bodylist_to_headform(Fs, F2).

negate(true, false) :-
	!.
negate(false, true) :-
	!.
negate(~F, F) :-
	!.
negate(F, ~F).

negate_deep(true, false) :-
	!.
negate_deep(false, true) :-
	!.
negate_deep(~((F,G)), (F1;G1)) :-
	!,
	negate_deep(F,F1),
	negate_deep(G,G1).
negate_deep(~((F;G)), (F1,G1)) :-
	!,
	negate_deep(F,F1),
	negate_deep(G,G1).
negate_deep(~(~(F)), F1) :-
	negate_deep(F, F1).
negate_deep(~F, F) :-
	!.
negate_deep(F, ~F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% for debugging
%%%% 
verify_interpolant(F,G,I) :-
	f_signature(F, FP, FF),
	f_signature(G, GP, GF),
	f_signature(I, IP, IF),
	sort(FP, FP1),
	sort(GP, GP1),
	sort(IP, IP1),
	sort(FF, FF1),
	sort(GF, GF1),
	sort(IF, IF1),
	( ord_subset(IP1, FP1), ord_subset(IP1, GP1) -> true
	; err('Verify interpolant failed: shared predicate condition for ~q wrt. ~q and ~q',
	      [IP1, FP1, GP1])
	),
	( ord_subset(IF1, FF1), ord_subset(IF1, GF1) -> true
	; ord_intersection(FF1, GF1, SharedF),
	  ord_subtract(IF1, SharedF, NonSharedF),
	  err('Verify interpolant failed: shared constants/functions condition for ~q',
	      [NonSharedF])
	),
	( fol_status_prover9(~(F -> I), [], unsatisfiable) -> true
	; err('Verify interpolant failed F -> I: ~q -> ~q', [F, I])
	),
	( fol_status_prover9(~(I -> G), [], unsatisfiable) -> true
	; err('Verify interpolant failed I -> G: ~q -> ~q', [I, G])
	).
	  
