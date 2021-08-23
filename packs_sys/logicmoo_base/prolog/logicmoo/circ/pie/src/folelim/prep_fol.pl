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

:- module(prep_fol,
	  [elim_by_renaming_for_validity/2]).

:- use_module(logop_fol).
:- use_module(simp_fol, [logform_remove_void_quantifiers/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elim_by_renaming_for_validity((F -> G), (F1 -> G1)) :-
	!,
	logform_solquantifiers_to_front(F, F0),
	logform_solquantifiers_to_front(G, G0),
	logform_process_subforms_with_polarity_and_bindings(F0, ebr_antecedent, F1),
	logform_process_subforms_with_polarity_and_bindings(G0, ebr_consequent, G1).
elim_by_renaming_for_validity((F <- G), (F1 <- G1)) :-
	!,
	logform_solquantifiers_to_front(F, F0),
	logform_solquantifiers_to_front(G, G0),
	logform_process_subforms_with_polarity_and_bindings(F0, ebr_consequent, F1),
	logform_process_subforms_with_polarity_and_bindings(G0, ebr_antecedent, G1).
elim_by_renaming_for_validity(F, F1) :-
	logform_solquantifiers_to_front(F, F0),
	logform_process_subforms_with_polarity_and_bindings(F0, ebr_consequent, F1).

%% *** only rename if not in scope of any other binding!
%% (other techniques might be applied to move bindings outward,
%% e.g. flip ex/ex2, Ackermann's switching)
%%
ebr_consequent(ex2(X,F), n, [], F1) :- !, ebr1(X, F, F1).
ebr_consequent(all2(X,F), p, [], F1) :- !, ebr1(X, F, F1).
ebr_consequent(X, _, _, X).

ebr_antecedent(ex2(X,F), p, [], F1) :- !, ebr1(X, F, F1).
ebr_antecedent(all2(X,F), n, [], F1) :- !, ebr1(X, F, F1).
ebr_antecedent(X, _, _, X).

ebr1(X, F, F1) :-
	logupon_list(X, Xs),
	map_gen_predicate(Xs, Map),
	logform_rename_free_predicates(F, Map, pn, F1).

map_gen_predicate([X|Xs], [X-Pred|Xs1]) :-
	logform_gen_predicate(Pred),
	map_gen_predicate(Xs, Xs1).
map_gen_predicate([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_solquantifiers_to_front(F, F1) :-
	logform_remove_void_quantifiers(F, F3),
	logform_process_subforms(F3, solq_tf_1, F4),
	logform_gather_quantifiers(F4, F1).

%%%% 
%%%% some possible improvements:
%%%% - instead of append merge the prefixes
%%%% - depending on polarity prefer ex2 or all2
%%%% - let expansion of <-> depend on polarity
%%%% 

solq_tf_1(FG, FG1) :-
	FG =.. [Op,F,G],
	( Op = (',') ; Op = (';') ),
	!,
	sol_prefix(F, PF, F1),
	sol_prefix(G, PG, G1),
	append(PF, PG, PFG),
	FG2 =.. [Op, F1, G1],
	sol_apply_prefix(PFG, FG2, FG1).
solq_tf_1((F -> G), FG1) :-
	!,
	sol_prefix(F, NPF, F1),
	flip_sol_prefix(NPF, PF),
	sol_prefix(G, PG, G1),
	append(PF, PG, PFG),
	sol_apply_prefix(PFG, (F1->G1), FG1).
solq_tf_1((F <- G), FG1) :-
	!,
	sol_prefix(F, PF, F1),
	sol_prefix(G, NPG, G1),
	flip_sol_prefix(NPG, PG),	
	append(PF, PG, PFG),
	sol_apply_prefix(PFG, (F1 <- G1), FG1).
solq_tf_1((F <-> G), FG1) :-
	!,
	sol_prefix(F, PF, _),
	sol_prefix(G, PG, _),
	( PF = [], PG = [] ->
	  FG1 = (F <-> G)
	; logform_clean_vars(((F -> G),(F <- G)), FG2),
	  solq_tf_1(FG2, FG1)
	).
solq_tf_1(ex(X, F), F1) :-
	!,
	sol_prefix(F, PF, F2),
	sol_apply_prefix_ex(PF, X, F2, F1).
solq_tf_1(all(X, F), F1) :-
	!,
	sol_prefix(F, PF, F2),
	sol_apply_prefix_all(PF, X, F2, F1).
solq_tf_1(~F, F1) :-
	!,
	sol_prefix(F, NPF, F2),
	flip_sol_prefix(NPF, PF),
	sol_apply_prefix(PF, ~F2, F1).
solq_tf_1(F, F).

sol_prefix(ex2(X, F), [ex2(X)|P], F1) :-
	!,
	sol_prefix(F, P, F1).
sol_prefix(all2(X, F), [all2(X)|P], F1) :-
	!,
	sol_prefix(F, P, F1).
sol_prefix(F, [], F).

sol_apply_prefix([ex2(X)|Ps], F, ex2(X, F1)) :-
	!,
	sol_apply_prefix(Ps, F, F1).
sol_apply_prefix([all2(X)|Ps], F, all2(X, F1)) :-
	!,
	sol_apply_prefix(Ps, F, F1).
sol_apply_prefix([], F, F).

sol_apply_prefix_ex([ex2(X)|Ps], Y, F, ex2(X, F1)) :-
	!,
	sol_apply_prefix_ex(Ps, Y, F, F1).
sol_apply_prefix_ex(Ps, Y, F, ex(Y, F1)) :-
	sol_apply_prefix(Ps, F, F1).

sol_apply_prefix_all([all2(X)|Ps], Y, F, all2(X, F1)) :-
	!,
	sol_apply_prefix_all(Ps, Y, F, F1).
sol_apply_prefix_all(Ps, Y, F, all(Y, F1)) :-
	sol_apply_prefix(Ps, F, F1).

flip_sol_prefix([all2(X)|P], [ex2(X)|P1]) :- !,	flip_sol_prefix(P, P1).
flip_sol_prefix([ex2(X)|P], [all2(X)|P1]) :- !,	flip_sol_prefix(P, P1).
flip_sol_prefix([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

