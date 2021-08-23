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

:- module(utils_fol,
	  [cnf1/2,
	   dnf1/2,
	   cnf2/2,
	   dnf2/2,
	   cnf3/2,
	   dnf3/2,
	   cnf4/2,
	   dnf4/2,
	   cnf5/2,
	   dnf5/2,
	   cnf6/2,
	   dnf6/2,
	   cnf_to_form/2,
	   dnf_to_form/2,
	   cnf_to_form_u/2,
	   dnf_to_form_u/2,
	   cnf_to_form_u1/2,
	   dnf_to_form_u1/2,
	   prettysort_matrix/2,
	   fol_red_unit/2,
	   m_simp_eq/2,
	   check_form/1]).

:- use_module(nf(nf)).
:- use_module(nf(nfutils)).
:- use_module(folelim(logop_fol)).
:- use_module(folelim(prettysymbols_fol)).
:- use_module(folelim(unskolemize)).
:- use_module(folelim(preprocexp), [m_red_condense/2, m_red_subsres/2]).
:- use_module(toytools(auxiliary), [cnf_prop/2, dnf_prop/2]).

check_form(F) :-
	f_signature(F, Ps, Fs),
	format(user_error, 'Predicates: ~q~n', [Ps]),
	format(user_error, 'Functions: ~q~n', [Fs]),
	( member(X/_, Ps), memberchk(X/_, Fs) ->
	  format(user_error, 'WARNING: Symbol used as predicate and function: ~q~n', [X])
	; true
	),
	( member(X/N, Ps), member(X/M, Ps), N \= M ->
	  format(user_error, 'WARNING: Predicate used with different arities: ~q/~q and /~q~n',
		 [X, N, M])
	; true
	),
	( member(X/N, Ps), ( logop_constant(X)
			   ; logop_unary(X)
			   ; logop_binary(X)
			   ; logop_quantifier(X)) ->
	  format(user_error, 'WARNING: Predicate with symbol of logic operator: ~q', [X/N])
	; true
	).

cnf1(F, M) :-
	cnf(F, M1),
	prettysort_matrix(M1, M).

dnf1(F, M) :-
	dnf(F, M1),
	prettysort_matrix(M1, M).

prettysort_matrix(M, M1) :-
	map_prettysort_clause(M, M2),
	sort(M2, M3),
	map_val(M3, M1).

prettysort_clause(C, C1, Key) :-
	length(C, Key),
	map_lit_sortkey(C, C2),
	sort(C2, C3),
	map_val(C3, C1).

lit_sortkey(~(X=Y), k(0,99999,z)-(~(X=Y))) :-
	!.
lit_sortkey(X=Y, k(1,99999,z)-(X=Y)) :-
	!.
lit_sortkey(~(A), k(0,Arity,Pred)-(~(A))) :-
	!,
	functor(A, Pred, Arity).
lit_sortkey(A, k(1,Arity,Pred)-A) :-
	functor(A, Pred, Arity).

% lit_sortkey(~(X=Y), k(z(z),2,0)-(~(X=Y))) :-
% 	!.
% lit_sortkey(X=Y, k(z(z),2,1)-(X=Y)) :-
% 	!.
% lit_sortkey(~(A), k(Pred,Arity,0)-(~(A))) :-
% 	!,
% 	functor(A, Pred, Arity).
% lit_sortkey(A, k(Pred,Arity,1)-A) :-
% 	functor(A, Pred, Arity).

map_lit_sortkey([X|Xs], [X1|Xs1]) :-
	lit_sortkey(X, X1),
	map_lit_sortkey(Xs, Xs1).
map_lit_sortkey([], []).

map_prettysort_clause([X|Xs], [Key-X1|Xs1]) :-
	prettysort_clause(X, X1, Key),
	map_prettysort_clause(Xs, Xs1).
map_prettysort_clause([], []).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Like cnf1 but performs also some equality simplifications,
%%%% in particular inlining of reflexivity
%%%% 
cnf2(F, M) :-
	cnf(F, M1),
	m_simp_eq(M1, cnf, M).

dnf2(F, M) :-
	dnf(F, M1),
	m_simp_eq(M1, dnf, M).

%%%% 
%%%% Like cnf2 but performs further simplifications
%%%% TODO: add more here: unit propagation, subsumption resolution
%%%%
cnf3(F, M) :-
	cnf(F, M1),
	m_simp_eq(M1, cnf, M2),
	m_red_condense(M2, M).

dnf3(F, M) :-
	dnf(F, M1),
	m_simp_eq(M1, dnf, M2),
	m_red_condense(M2, M).

m_simp_eq(M, M1) :-
	m_simp_eq(M, cnf, M1).

m_simp_eq(M, NF, M1) :-
	map_c_inline_eq(M, NF, M2),
	map_c_apply_eq(M2, NF, M2a),
	map_c_orient_eq(M2a, M3),
	fol_red_taut(M3, M4),
	m_simp_eqtaut(M4, NF, M5),
	m_simp_eqvoid(M5, NF, M6),
	fol_red_unit(M6, M6a),
	fol_sort_lits_by_pred_occurrences(M6a, M7),
	fol_red_subs(M7, M8),
	prettysort_matrix(M8, M9),
	m_reuse_vars(M9, M1).

m_simp_eqtaut([C|Cs], cnf, Cs1) :-
	member(A=B, C),
	A == B,
	!,
	m_simp_eqtaut(Cs, cnf, Cs1).
m_simp_eqtaut([C|Cs], dnf, Cs1) :-
	member(~(A=B), C),
	A == B,
	!,
	m_simp_eqtaut(Cs, dnf, Cs1).
m_simp_eqtaut([C|Cs], NF, [C|Cs1]) :-
	m_simp_eqtaut(Cs, NF, Cs1).
m_simp_eqtaut([], _, []).

m_simp_eqvoid([C|Cs], NF, [C1|Cs1]) :-
	c_simp_eqvoid(C, NF, C1),
	m_simp_eqvoid(Cs, NF, Cs1).
m_simp_eqvoid([], _, []).

c_simp_eqvoid([(A=B)|Xs], cnf, Xs1) :-
	A == B,
	!,
	c_simp_eqvoid(Xs, cnf, Xs1).
c_simp_eqvoid([~(A=B)|Xs], dnf, Xs1) :-
	A == B,
	!,
	c_simp_eqvoid(Xs, dnf, Xs1).
c_simp_eqvoid([X|Xs], NF, [X|Xs1]) :-
	c_simp_eqvoid(Xs, NF, Xs1).
c_simp_eqvoid([], _, []).

map_c_orient_eq([X|Xs], [X1|Xs1]) :-
	map_l_orient_eq(X, X2),
	%% include MULT simplification:
	sort(X2, X1), 
	map_c_orient_eq(Xs, Xs1).
map_c_orient_eq([], []).

map_l_orient_eq([X|Xs], [X1|Xs1]) :-
	l_orient_eq(X, X1),
	map_l_orient_eq(Xs, Xs1).
map_l_orient_eq([], []).

l_orient_eq(A=B, X) :-
	term_less(B, A),
	!,
	X = (B=A).
l_orient_eq(~(A=B), X) :-
	term_less(B, A),
	!,
	X = ~(B=A).
l_orient_eq(X, X).

%%%% 
%%%% Should ensure stability wrt namings of Skolem terms, e.g. avoid sk10 @< sk9
%%%% 
term_less(X, Y) :-
	skterm_arity_number(X, A1, N1),
	skterm_arity_number(Y, A2, N2),
	!,
	A1-N1 @< A2-N2.
term_less(X, Y) :-
	X @< Y.

skterm_arity_number(T, A, N) :-
	\+ var(T),
	functor(T, F, A),
	atom(F),
	atom_concat(sk, N1, F),
	atom_number(N1, N).

map_c_inline_eq([X|Xs], NF, [X1|Xs1]) :-
	copy_term(X, X2),
	c_inline_eq(X2, NF, X1),
	map_c_inline_eq(Xs, NF, Xs1).
map_c_inline_eq([], _, []).

c_inline_eq([~(X = Y)|Xs], cnf, Xs1) :-
	unify_with_occurs_check(X, Y),
	!,
	c_inline_eq(Xs, cnf, Xs1).
c_inline_eq([(X = Y)|Xs], dnf, Xs1) :-
	unify_with_occurs_check(X, Y),
	!,
	c_inline_eq(Xs, dnf, Xs1).
c_inline_eq([X|Xs], NF, [X|Xs1]) :-
	c_inline_eq(Xs, NF, Xs1).
c_inline_eq([], _, []).

map_c_apply_eq([X|Xs], Y1, [X1|Xs1]) :-
	c_apply_eq(X, Y1, X1),
	map_c_apply_eq(Xs, Y1, Xs1).
map_c_apply_eq([], _, []).

c_apply_eq(C, NF, C1) :-
	c_extract_eq(C, NF, Map),
	map_l_rewrite(C, NF, Map, C1).

c_extract_eq(C, NF, Map) :-
	c_ee(C, NF, [], EquiClasses),
	eqc_to_map(EquiClasses, Map).

eqc_to_map([E|Es], M) :-
	sort(E, [Y|Xs]),
	eqc_to_map_1(Xs, Y, M, M1),
	eqc_to_map(Es, M1).
eqc_to_map([], []).

eqc_to_map_1([X|Xs], Y, [X-Y|M1], M2) :-
	eqc_to_map_1(Xs, Y, M1, M2).
eqc_to_map_1([], _, M, M).

l_equates(cnf, ~(X=Y), X, Y).
l_equates(dnf, (X=Y), X, Y).

c_ee([L|C], NF, E, E1) :-
	l_equates(NF, L, X, Y),
	!,
	( select(K1, E, E2),
	  member(X1, K1),
	  X1 == X ->
	  ( member(Y1, K1),
	    Y1 == Y ->
	    E0 = E
	  ; select(K2, E2, E3),
	    member(Y1, K2),
	    Y1 == Y ->
	    append(K1, K2, K3),
	    E0 = [K3|E3]
	  ; E0 = [[Y|K1]|E2]
	  )
	; select(K2, E, E2),
	  member(Y1, K2),
	  Y1 == Y ->
	  E0 = [[X|K2]|E2]
	; E0 = [[X,Y]|E]
	),
	c_ee(C, NF, E0, E1).
c_ee([_|C], NF, E, E1) :-
	c_ee(C, NF, E, E1).
c_ee([], _, E, E).
	
map_l_rewrite([X|Xs], NF, Y1, [X1|Xs1]) :-
	l_rewrite(NF, X, Y1, X1),
	map_l_rewrite(Xs, NF, Y1, Xs1).
map_l_rewrite([], _, _, []).

l_rewrite(NF, L, _, L) :-
	l_equates(NF, L, _, _),
	!.
l_rewrite(_, L, Map, L1) :-
	l_rewrite_1(L, Map, L1).

map_l_rewrite_1([X|Xs], Y1, [X1|Xs1]) :-
	l_rewrite_1(X, Y1, X1),
	map_l_rewrite_1(Xs, Y1, Xs1).
map_l_rewrite_1([], _, []).

l_rewrite_1(T, Map, T1) :-
	member(T2-T1, Map),
	T == T2,
	!.
l_rewrite_1(T, Map, T1) :-
	compound(T),
	!,
	T =.. [F|Ts],
	map_l_rewrite_1(Ts, Map, Ts1),
	T1 =.. [F|Ts1].
l_rewrite_1(T, _, T).

%%%%
%%%% m_reuse_vars(+MatrixIn, -MatrixOut)
%%%%
%%%% - Clauses in MatrixOut share variables to keep
%%%%   the overall number of variables in the matrix
%%%%   low
%%%%
%%%% - Variables in MarixOut are fresh
%%%%
m_reuse_vars(M, M1) :-
	m_reuse_vars_1(M, _, M1).

m_reuse_vars_1([C|Cs], Vs, [C1|Cs1]) :-
	copy_term(C, C1),
	term_variables(C1, Vs, _),
	m_reuse_vars_1(Cs, Vs, Cs1).
m_reuse_vars_1([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cnf_to_form(M, F) :-
	m_reuse_vars(M, M1),
	m_signature(M1, _, Fs),
	map_list_to_disjunction(M1, M2),
	list_to_conjunction(M2, F1),
	term_variables(F1, Vs),
	( Vs = [] ->
	  F = F1
	; F = all(Vs, F1)
	),
	instantiate_vars(Vs, Fs).

dnf_to_form(M, F) :-
	m_reuse_vars(M, M1),
	m_signature(M1, _, Fs),
	map_list_to_conjunction(M1, M2),
	list_to_disjunction(M2, F1),
	term_variables(F1, Vs),
	( Vs = [] ->
	  F = F1
	; F = ex(Vs, F1)
	),
	instantiate_vars(Vs, Fs).

cnf_to_form_u(M, F) :-
	cnf_to_form(M, F1),
	unskolemize(F1, [], F2, S),
	( S = success ->
	  logform_vars_to_pretty_symbols(F2, F)
	; F = F1
	).

dnf_to_form_u(M, F) :-
	dnf_to_form(M, F1),
	unskolemize(~F1, [], F2, S),
	( S = success ->
	  logform_negate(F2, F2a),
	  logform_vars_to_pretty_symbols(F2a, F)
	; F = F1
	).

cnf_to_form_u1(M, F) :-
	cnf_to_form(M, F1),
	unskolemize(F1, [nosepfun], F2, S),
	( S = success ->
	  logform_vars_to_pretty_symbols(F2, F)
	; F = F1
	).

dnf_to_form_u1(M, F) :-
	dnf_to_form(M, F1),
	unskolemize(~F1, [nosepfun], F2, S),
	( S = success ->
	  logform_negate(F2, F2a),
	  logform_vars_to_pretty_symbols(F2a, F)
	; F = F1
	).

map_list_to_disjunction([X|Xs], [X1|Xs1]) :-
	list_to_disjunction(X, X1),
	map_list_to_disjunction(Xs, Xs1).
map_list_to_disjunction([], []).

map_list_to_conjunction([X|Xs], [X1|Xs1]) :-
	list_to_conjunction(X, X1),
	map_list_to_conjunction(Xs, Xs1).
map_list_to_conjunction([], []).

instantiate_vars(Xs, Fs) :-
	instantiate_vars_1(Xs, 0, Fs).

instantiate_vars_1([X|Xs], N, Fs) :-
	next_var(N, Fs, X, N1),
	instantiate_vars_1(Xs, N1, Fs).
instantiate_vars_1([], _, _).

next_var(N, Fs, V, N1) :-
	nth_var(N, V1),
	\+ memberchk(V1/_, Fs),
	!,
	V = V1,
	N1 is N+1.
next_var(N, Fs, V, N1) :-
	N2 is N+1,
	next_var(N2, Fs, V, N1).

nth(0, [X|_], X) :- !.
nth(N, [_|Xs], X) :-
	N > 0,
	N1 is N-1,
	nth(N1, Xs, X).

nth_var(N, V) :-
 	S = [x,y,z,u,v,w],
	length(S, L),
	I is N mod L,
	nth(I, S, V1),
	J is N div L,
	( J > 0 ->
	  concat_atom([V1, J], V)
	; V = V1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_unit_complements([], []).
m_unit_complements([[U]|M], [CU|CUs]) :-
	!,
	mlit_complem_1(U, CU),
	m_unit_complements(M, CUs).
m_unit_complements([_|M], CUs) :-
	m_unit_complements(M, CUs).
	
fol_red_unit(M, M1) :-
	m_reuse_vars(M, M2),
	m_unit_complements(M2, NUs1),
	sort(NUs1, NUs2),
	findall(NU, member(NU, NUs2), NUs3),
	fol_red_unit_1(M2, NUs3, M1).

fol_red_unit_1(M, [], M) :-
	!.
fol_red_unit_1(M, NUs, M1) :-
	map_fol_red_unit_c(M, NUs, NUs1, M2),
	fol_red_unit_1(M2, NUs1, M1).

map_fol_red_unit_c([C|M], NUs, NUs1, [C1|M1]) :-
	subtract_units(C, NUs, C1),
	( C1 = [U], C1 \= C ->
	  mlit_complem_1(U, NU),
	  copy_term(NU, NU1),
	  NUs1 = [NU1|NUs2]
	; NUs1 = NUs2
	),
	map_fol_red_unit_c(M, NUs, NUs2, M1).
map_fol_red_unit_c([], _, [], []).
		
mlit_complem_1(~A, A) :-
	!.
mlit_complem_1(A, ~A).

subtract_units([L|Ls], NUs, Ls1) :-
	member(L1, NUs),
	subsumes_chk(L1, L),
	!,
	subtract_units(Ls, NUs, Ls1).
subtract_units([L|Ls], NUs, [L|Ls1]) :-
	subtract_units(Ls, NUs, Ls1).
subtract_units([], _, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cnf4(F, M) :-
	cnf_v2(F, M1),
	fol_sort_lits_by_pred_occurrences(M1, M1a),
	fol_red_subs(M1a, M).

dnf4(F, M) :-
	dnf_v2(F, M1),
	fol_sort_lits_by_pred_occurrences(M1, M1a),
	fol_red_subs(M1a, M).

cnf5(F, M) :-
	cnf_v2(F, M1),
	fol_sort_lits_by_pred_occurrences(M1, M1a),
	fol_red_subs(M1a,M1b),
	m_simp_eq(M1b, cnf, M2),
	m_red_condense(M2, M).

dnf5(F, M) :-
	dnf_v2(F, M1),
	fol_sort_lits_by_pred_occurrences(M1, M1a),
	fol_red_subs(M1a,M1b),
	m_simp_eq(M1b, dnf, M2),
	m_red_condense(M2, M).

cnf6(F, M) :-
	cnf_v2(F, M1),
	fol_sort_lits_by_pred_occurrences(M1, M1a),
	fol_red_subs(M1a,M1b),
	m_simp_eq(M1b, cnf, M2),
	m_red_condense(M2, M3),
	m_red_subsres(M3, M).

dnf6(F, M) :-
	dnf_v2(F, M1),
	fol_sort_lits_by_pred_occurrences(M1, M1a),
	fol_red_subs(M1a,M1b),
	m_simp_eq(M1b, dnf, M2),
	m_red_condense(M2, M3),
	m_red_subsres(M3, M).

dnf_v2(X,Y) :- 
	and_or_nf(X,X1), 
	vars_to_prolog(X1,X2), 
	pnf(X2,X3), 
	skolem_all(X3,X31),
	term_variables(X31, Vs),
	map_gen_function(Vs, RevMap),
	dnf_prop(X31, X4),
	( RevMap = [] ->
	  Y = X4
	; logform_rename_free_functions(X4, RevMap, Y)
	).

cnf_v2(X,Y) :- 
	and_or_nf(X,X1), 
	vars_to_prolog(X1,X2), 
	pnf(X2,X3), 
	skolem_ex(X3,X31),
	term_variables(X31, Vs),
	map_gen_function(Vs, RevMap),
	cnf_prop(X31, X4),
	( RevMap = [] ->
	  Y = X4
	; logform_rename_free_functions(X4, RevMap, Y)
	).

map_gen_function([V|Vs], [V-_|Map]) :-
	logform_gen_function(V),
	map_gen_function(Vs, Map).
map_gen_function([], []).

skolem_ex(P,P1) :- skolem_ex(P,[],P1).
skolem_all(P,P1) :- skolem_all(P,[],P1).

skolem_ex(all(X,P),Vs,P1) :- !, skolem_ex(P,[X|Vs],P1).
skolem_ex(ex(X,P),Vs,P1) :- !, skterm(Vs,X), skolem_ex(P,Vs,P1).
skolem_ex(P,_,P).

skolem_all(ex(X,P),Vs,P1) :- !, skolem_all(P,[X|Vs],P1).
skolem_all(all(X,P),Vs,P1) :- !, skterm(Vs,X), skolem_all(P,Vs,P1).
skolem_all(P,_,P).

skterm(Vs,T) :-	gen_sk_symbol(SK), T =.. [SK|Vs].
	
