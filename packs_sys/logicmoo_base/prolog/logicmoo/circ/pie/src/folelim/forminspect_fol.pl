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

%%%% 
%%%% EXPERIMENTAL, NOT TESTED
%%%% 

:- module(forminspect_fol,
 	  [is_restricted_conradie/2,
 	   is_independent_conradie/3,
	   standardize_conradie/3,
	   standardize_conradie_strongly/3,
	   malignant/3,
	   non_benign/4,
	   in_good_scope/2,
	   contains_predicate_outof_ea_scope/3]).


:- use_module(swilib(graphs)).
:- use_module(logop_fol).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reset_local_gensyms :-
	logform_set_p_counter(0),
	logform_set_x_counter(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_restricted_conradie(F, Ps) :-
	\+ is_not_restricted_1(F, Ps),
	\+ ( member(P, Ps),
	     contains_predicate_outof_ea_scope(F, P, p)
	   ).

is_not_restricted_1(F, Ps) :-
	logform_enum_subformulas(F, all(_, F1)),
	findall(k, ( member(P, Ps),
		     logform_enum_atoms(F1, A, p),
		     A =.. [P|_] ),
		[_,_|_]),
	!.

is_independent_conradie(F, Ps, Ps1) :-
	findall(Edge, ddc(F, Ps, Edge), Edges),
	p_to_s_graph(Edges, S),
	%% top_sort from graphs.pl seems to fail in case the graph is cyclic,
	%% thus there is no "is_acyclic" test here.
	top_sort(S, Ps1).

ddc(F, Ps, PI-PJ) :-
	logform_enum_subformulas(F, all(_, C)),
	member(PI, Ps),
	contains_predicate(C, PI, n),
	member(PJ, Ps),
	contains_predicate(C, PJ, p).

% is_acyclic(Edges) :-
% 	\+ is_cyclic(Edges).
% 
% is_cyclic(Edges) :-
% 	p_to_s_graph(Edges, S),
% 	warshall(S, S1),
% 	member(X-Xs, S1),
% 	memberchk(X, Xs),
% 	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

standardize_conradie(F, Ps, NNF) :-
	reset_local_gensyms,
	logform_to_nnf(F, F1),
	logform_clean_vars(F1, F2),
	narrow_quantifiers(F2, Ps, NNF).

standardize_conradie_strongly(F, Ps, NNF) :-
	reset_local_gensyms,
	logform_to_nnf(F, F1),
	logform_clean_vars(F1, F2),
	narrow_quantifiers_strongly(F2, Ps, F3),
	%% need to clean again because variables might be duplicated in
	%% propagating "all" over conjunction:
	reset_local_gensyms,
	logform_clean_vars(F3, NNF).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Narrow Quantifiers
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% - Move quantifiers inwards if they do not occur in one operand.
%%%% - Remove quantification upon variables that do not occur in the
%%%%   argument formula.
%%%% - "Strong" version that also moves universal quantification into
%%%%   conjunction.
%%%% - Only applied to subformulas containing a predicate in the given
%%%%   list of predicates. (Empty list for "all predicates".)
%%%%
%%%% - Assumes "clean" and NNF input (?)
%%%%
%%%% Note: Seems hard to fit into a a general subterm processing scheme.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


narrow_quantifiers(F, Ps, F) :-
	Ps \= [],
	\+ ( member(P, Ps), contains_predicate(F, P) ),
	!.
narrow_quantifiers(QFG, Ps, QFG1) :-
	quantifier_op_decompose(QFG, Q, Op, X, F, G),
	!,
	logupon_list(X, Xs),
	narrow_term_quantifiers_1(Xs, F, G, FXs, GXs, FGXs),
	quantifier_compose(Q, FXs, F, F1),
	quantifier_compose(Q, GXs, G, G1),				    
	narrow_quantifiers(F1, Ps, F2),
	narrow_quantifiers(G1, Ps, G2),
	quantifier_op_compose(Q, Op, FGXs, F2, G2, QFG1).
narrow_quantifiers(QF, _, QF1) :-
	quantifier_decompose(QF, Q, X, F),
	!,
	logupon_list(X, Xs),
	remove_unused_term_quantifiers(Xs, F, FXs),
	quantifier_compose(Q, FXs, F, QF1).
narrow_quantifiers((F,G), Ps, (F1,G1)) :-
	!,
	narrow_quantifiers(F, Ps, F1),
	narrow_quantifiers(G, Ps, G1).
narrow_quantifiers((F;G), Ps, (F1;G1)) :-
	!,
	narrow_quantifiers(F, Ps, F1),
	narrow_quantifiers(G, Ps, G1).
narrow_quantifiers(F, _, F).

narrow_term_quantifiers_1([X|Xs], F, G, FXs, GXs, FGXs) :-
	( contains_termfunctor(F, X) ->
	  ( contains_termfunctor(G, X) ->
	    FGXs = [X|FGXs1],
	    FXs = FXs1,
	    GXs = GXs1
	  ; FGXs = FGXs1,
	    FXs = [X|FXs1],
	    GXs = GXs1
	  )
	; contains_termfunctor(G, X) ->
	  FGXs = FGXs1,
	  FXs = FXs1,
	  GXs = [X|GXs1]
	; FGXs = FGXs1,
	  FXs = FXs1,
	  GXs = GXs1
	),
	narrow_term_quantifiers_1(Xs, F, G, FXs1, GXs1, FGXs1).
narrow_term_quantifiers_1([], _, _, [], [], []).

narrow_quantifiers_strongly(F, Ps, F) :-
	Ps \= [],
	\+ ( member(P, Ps), contains_predicate(F, P) ),
	!.
narrow_quantifiers_strongly(QFG, Ps, QFG1) :-
	quantifier_op_decompose(QFG, all, and, Xs, F, G),
	!,
	narrow_quantifiers_strongly(all(Xs, F), Ps, F1),
	narrow_quantifiers_strongly(all(Xs, G), Ps, G1),
	QFG1 = (F1,G1).
narrow_quantifiers_strongly(F, Ps, F1) :-
	narrow_quantifiers(F, Ps, F1).

remove_unused_term_quantifiers([X|Xs], F, FXs) :-
	( contains_termfunctor(F, X) ->
	  FXs = [X|FXs1]
	; FXs = FXs1
	),
	remove_unused_term_quantifiers(Xs, F, FXs1).
remove_unused_term_quantifiers([], _, []).

contains_termfunctor(F, X) :-
	logform_enum_atoms(F, A),
	A =.. [_|Args],
	map_ctf_1(Args, X).

map_ctf_1([X|Xs], Y) :-
	X =.. [X1|Args],
	( X1 == Y -> true
	; map_ctf_1(Args, Y) ->
	  true
	; map_ctf_1(Xs, Y)
	).

quantifier_decompose(ex(X, F), ex, X, F) :- !.
quantifier_decompose(ex(X, F), ex, X, F) :- !.
quantifier_decompose(all(X, F), all, X, F) :- !.
quantifier_decompose(all(X, F), all, X, F) :- !.

quantifier_compose(_, [], F, F) :- !.
quantifier_compose(ex, Xs, F, ex(Xs, F)) :- !.
quantifier_compose(all, Xs, F, all(Xs, F)) :- !.

quantifier_op_decompose(ex(X, (F,G)), ex, and, X, F, G) :- !.
quantifier_op_decompose(ex(X, (F;G)), ex, or, X, F, G) :- !.
quantifier_op_decompose(all(X, (F,G)), all, and, X, F, G) :- !.
quantifier_op_decompose(all(X, (F;G)), all, or, X, F, G) :- !.

quantifier_op_compose(_, and, [], F, G, (F,G)) :- !.
quantifier_op_compose(_, or, [], F, G, (F;G)) :- !.
quantifier_op_compose(ex, and, Xs, F, G, ex(Xs, (F,G))) :- !.
quantifier_op_compose(ex, or, Xs, F, G, ex(Xs, (F;G))) :- !.
quantifier_op_compose(all, and, Xs, F, G, all(Xs, (F,G))) :- !.
quantifier_op_compose(all, or, Xs, F, G, all(Xs, (F;G))) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% These just apply to first-order NNFs (NNF in the sense of logform_to_nnf)
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

malignant(C, D, P) :-
	contains_predicate(C, P, p),
	contains_predicate(D, P, n).
malignant(C, D, P) :-
	contains_predicate(C, P, n),
	contains_predicate(D, P, p).

non_benign(C, D, P, Pol) :-
	contains_predicate(C, P, Pol),
	contains_predicate(D, P),
	!.
non_benign(C, D, P, Pol) :-
	contains_predicate(D, P, Pol),
	contains_predicate(C, P),
	!.

in_good_scope(F, P) :-
	\+ in_bad_scope(F, P).

in_bad_scope(F, P) :-
	logform_enum_subformulas(F, all(_, F1)),
	logform_enum_subformulas(F1, F2),
	( F2 = (C,D) ; F2  = (C;D) ),
	malignant(C, D, P),
	!.
in_bad_scope(F, P) :-
	logform_enum_subformulas(F, (B1,B2)),
	( contains_predicate_outof_ea_scope(B1, P, p),
  	  contains_predicate_outof_ea_scope(B2, P, n)
	; contains_predicate_outof_ea_scope(B1, P, n),
  	  contains_predicate_outof_ea_scope(B2, P, p)
	),
	!.

contains_predicate(F, P, Pol) :-
	logform_enum_atoms(F, A, Pol),
	functor(A, P, _),
	!.

contains_predicate(F, P) :-
	logform_enum_atoms(F, A),
	functor(A, P, _),
	!.

contains_predicate_outof_ea_scope(F, P, Pol) :-
	logform_enum_subformulas(F, all(_, F1)),
	contains_predicate_outof_ea_scope_1(F1, P, Pol).

contains_predicate_outof_ea_scope_1(F, P, Pol) :-
	logform_enum_subformulas(F, ex(_, F1)),
	contains_predicate(F1, P, Pol),
	!.
contains_predicate_outof_ea_scope_1(F, P, Pol) :-
	logform_enum_subformulas(F, (F1;F2)),
	non_benign(F1, F2, P, Pol),             %% CHECK Pol here!
	!.
contains_predicate_outof_ea_scope_1(F, P, _) :-
	logform_enum_subformulas(F, (F1,F2)),
	malignant(F1, F2, P),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Enumerate conjuncts of the formula
%%%% - e.g. to detect explicit definition
%%%%
fol_enum_conjuncts(F, G) :-
	empty_prefix(Q),
	fec(F, Q, G).

fec((F1,F2), Q, G) :- !, (fec(F1, Q, G) ; fec(F2, Q, G)).
fec(all(X, F), Q, G) :- !, append_to_prefix(X, Q, Q1), fec(F, Q1, G).
fec(~ex(X, F), Q, G) :- !, fec(all(X, ~F), Q, G).
fec(~(~(F)), Q, G) :- !, fec(F, Q, G).
fec(~(F1;F2), Q, G) :- !, fec((~F1,~F2), Q, G).
fec(~(F1 -> F2), Q, G) :- !, fec((F1,~F2), Q, G).
fec(~(F1 <- F2), Q, G) :- !, fec((~F1,F2), Q, G).
fec(~(F1 <-> F2), Q, G) :- !, fec((F1 <-> ~F2), Q, G).
fec(F, Q, G) :-	!, apply_prefix(Q, F, G).

empty_prefix([]).
append_to_prefix(X, Q, Q1) :-
	( X = [] ->
	  Q1 = Q
	; X = [_|_] ->
	  append(X, Q, Q1)
	; Q1 = [X|Q]
	).
apply_prefix([], F, F) :-
	!.
apply_prefix(Q, F, all(X, F)) :-
	reverse(Q, X).
	

% ? conditional definition p -> (<->)

% p(P,_), tptp_problem(P, [normalize=false], _,B), fol_enum_conjuncts(B,C), C=all(_,D), functor(D,F,N), writeln(F/N), fail.