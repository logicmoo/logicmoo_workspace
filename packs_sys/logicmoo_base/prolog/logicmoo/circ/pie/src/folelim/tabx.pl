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

:- module(tabx,
	  [tabx_to_interpolant/5,
	   tabx_to_interpolant/4,
	   tabx_to_prop_interpolant/3,
	   tabx_to_prop_interpolant/2,
	   tabx_to_matrices/3]).

:- use_module(swilib(options)).
:- use_module(swilib(err)).
:- use_module(swilib(graphs)).
:- use_module(nf(nfutils)).
:- use_module(nf(nf)).
:- use_module(prettysymbols_fol).
:- use_module(simp_fol).

:- use_module(toytools(toyproject), [elim/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Tabx Format
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% A "Tabx" is a List of Tableau (no artificial root node and there might be
%%%% several roots due to branching in the first step.)
%%%%
%%%% Tableau := tab(Formula, Color, ListOfTableau) |
%%%%            closed(Formula, Color1, Color2)
%%%% 
%%%% - tab(Formula, Color, ListOfTableau) represents a node, labelled by
%%%%   Formula in Color, and with children ListOfTableau.
%%%%
%%%% - closed(Form, Color1, Color2) means that the branch is closed by two
%%%%   ancestors with Form in Color1 and Form2 in Color2, where Form2 implies
%%%%   ~Form.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Basically the method in Fitting's book
%%%%

tabx_to_prop_interpolant(Tabx, F) :-
	tabx_to_prop_interpolant(Tabx, [], F).

tabx_to_prop_interpolant([], _Options, true) :-
	!.
tabx_to_prop_interpolant(Tabx, Options, F) :-
	copy_term(Tabx, Tabx1),
	term_variables(Tabx1, Vs),
	set_vars_to_k(Vs),
	tabx_to_prop_interpolant_1(Tabx1, Options, F1),
	from_options(ip_simp_prop_method=SimpPropMethod, Options, cnf),
	simp_fprop(SimpPropMethod, F1, F).

set_vars_to_k(['$k'|Xs]) :-
	set_vars_to_k(Xs).
set_vars_to_k([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tabx_to_matrices_1(Tabx, [CA|MA], MB) :-
	tabx_color(Tabx, a),
	!,
	map_tab_literal(Tabx, CA),
	map_sub_tabx_to_matrices(Tabx, MA, MB).
tabx_to_matrices_1(Tabx, MA, [CB|MB]) :-
	tabx_color(Tabx, b),
	!,
	map_tab_literal(Tabx, CB),
	map_sub_tabx_to_matrices(Tabx, MA, MB).
tabx_to_matrices_1([closed(_,_,_)], [], []).

map_sub_tabx_to_matrices([tab(_,_,Tx)|Tx1], MA, MB) :-
	tabx_to_matrices(Tx, MA1, MB1),
	map_sub_tabx_to_matrices(Tx1, MA2, MB2),
	append(MA1, MA2, MA),
	append(MB1, MB2, MB).
map_sub_tabx_to_matrices([], [], []).

map_tab_literal([tab(L,_,_)|Tx], [L|Ls]) :-
	map_tab_literal(Tx, Ls).
map_tab_literal([], []).

tabx_to_matrices([], [], []) :-
	!.
tabx_to_matrices(Tabx, MA, MB) :-
	tabx_to_matrices_1(Tabx, MA, MB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tabx_to_prop_interpolant_1(Tabx, Os, F) :-
	tabx_color(Tabx, a),
	!,
	map_tab_to_prop_interpolant_1(Tabx, Os, Forms),
	formlist_to_disjunction(Forms, F).
tabx_to_prop_interpolant_1(Tabx, Os, F) :-
	tabx_color(Tabx, b),	
	!,
	map_tab_to_prop_interpolant_1(Tabx, Os, Forms),
	formlist_to_conjunction(Forms, F).
tabx_to_prop_interpolant_1([closed(F1,C1,C2)], Os, F) :-
	!,
	tab_to_prop_interpolant_1(closed(F1,C1,C2), Os, F).
tabx_to_prop_interpolant_1(X, _, _) :-
	err('tabx_to_prop_interpolant not applicable to ~q', [X]).

tabx_color([tab(_,Color,_)|_], Color).

tab_to_prop_interpolant_1(tab(_,_,Tabx), Os, F) :-
	!,
	tabx_to_prop_interpolant_1(Tabx, Os, F).
tab_to_prop_interpolant_1(closed(F1,C1,C2), _Os, F) :-
	!,
	(C1 = a ->
	  ( C2 = a -> F = false ; F = F1 )
	; ( C2 = a -> complement(F1,F) ; F = true )
	).
tab_to_prop_interpolant_1(X, _, _) :-
	err('tab_to_interpolant not applicable to ~q', [X]).

map_tab_to_prop_interpolant_1([X|Xs], Os, [X1|Xs1]) :-
	tab_to_prop_interpolant_1(X, Os, X1),
	map_tab_to_prop_interpolant_1(Xs, Os, Xs1).
map_tab_to_prop_interpolant_1([], _, []).

complement(~F, F) :-
	!.
complement(F, ~F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

formlist_to_conjunction(Fs, F) :-
	sort(Fs, Fs1),
	formlist_to_conjunction_1(Fs1, F).

formlist_to_conjunction_1([], true).
formlist_to_conjunction_1(L, false) :-
	memberchk(false, L),
	!.
formlist_to_conjunction_1(Fs, F) :-
	formlist_to_conjunction_2(Fs, F).
formlist_to_conjunction_2([F], F) :-
	!.
formlist_to_conjunction_2([true|Fs], F) :-
 	!,
 	formlist_to_conjunction_2(Fs, F).
formlist_to_conjunction_2([F,true|Fs], F1) :-
 	!,
 	formlist_to_conjunction([F|Fs], F1).
formlist_to_conjunction_2([F|Fs], (F,Fs1)) :-
	formlist_to_conjunction_2(Fs, Fs1).

formlist_to_disjunction(Fs, F) :-
	sort(Fs, Fs1),
	formlist_to_disjunction_1(Fs1, F).

formlist_to_disjunction_1([], false).
formlist_to_disjunction_1(L, true) :-
	memberchk(true, L),
	!.
formlist_to_disjunction_1(Fs, F) :-
	formlist_to_disjunction_2(Fs, F).
formlist_to_disjunction_2([F], F) :-
	!.
formlist_to_disjunction_2([false|Fs], F) :-
 	!,
 	formlist_to_disjunction_2(Fs, F).
formlist_to_disjunction_2([F,false|Fs], F1) :-
 	!,
 	formlist_to_disjunction([F|Fs], F1).
formlist_to_disjunction_2([F|Fs], (F;Fs1)) :-
	formlist_to_disjunction_2(Fs, Fs1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simp_ffol(none, F, F) :-
	!.
simp_ffol(fast, F, F1) :-
	!,
	simp_fol_fast(F, F1).
simp_ffol(cnf, F, F1) :-
	!,
	simp_fol_c5(F, F1).
simp_ffol(dnf, F, F1) :-
	!,
	simp_fol_d5(F, F1).
simp_ffol(Method, _, _) :-
	err('No known FOL simplification method: ~q', [Method]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Propositional simplification is allowed to handle equality (=/2) in the
%%%% first-order sense, in particular replacing an equality with identical
%%%% left and right side by true.
%%%% 

simp_fprop(none, F, F) :-
	!.
simp_fprop(te, F, F1) :-
	!,
	elim(F, F1).
simp_fprop(fast, F, F1) :-
	!,
	simp_fol_fast(F, F1).
simp_fprop(cnf, F, F1) :-
	!,
	cnf5(F, F2),
	matrix_to_form_keep_skolems(F2, F1).
simp_fprop(dnf, F, F1) :-
	!,
	dnf5(F, F2),
	matrix_to_form_keep_skolems(F2, F1).
simp_fprop(Method, _, _) :-
	err('No known simplification method: ~q', [Method]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% FIRST-ORDER CONVERSION
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tabx_to_interpolant(T, MA, MB, I) :-
	tabx_to_interpolant(T, MA, MB, [], I).

tabx_to_interpolant(T, MA, MB, Options, H) :-
	%%
	tabx_to_prop_interpolant(T, Options, H1),
	from_options(ip_prop=H1, Options, _),
	%%
	m_signature(MA, _, FA),
	m_signature(MB, _, FB),
	sort(FA, FA1),
	sort(FB, FB1),
	ord_subtract(FA1, FB1, FA2),
	ord_subtract(FB1, FA1, FB2),
	ipol_lift(H1, FA2, FB2, H2, TA, TB, TK),
	order_topterms(TA, TB, Options, Prefix),
	wrap_prefix(Prefix, H2, H3),
	map_val(TK, Ks),
	( Ks = [] ->
	  H4 = H3
	; from_options(ip_grounding_quantifier=KQuant, Options, ex),
	  ( Ks = [K1] ->
	    H4 =.. [KQuant, K1, H3]
	  ; H4 =.. [KQuant, Ks, H3]
	  )
	),
	logform_vars_to_pretty_symbols(H4, H5),
	from_options(ip_simp_fol_method=SimpFolMethod, Options, fast),
	simp_ffol(SimpFolMethod, H5, H).

ipol_lift(H, FA, FB, H1, TA, TB, TK) :-
	il_1(H, FA, FB, [], [], [], H1, TA, TB, TK).

il_1(H, FA, FB, TA, TB, TK, H1, TA1, TB1, TK1) :-
	il_logop(H, Op, H2),
	!,
	map_il_1(H2, FA, FB, TA, TB, TK, H3, TA1, TB1, TK1),
	H1 =.. [Op|H3].
il_1(H, FA, FB, TA, TB, TK, H1, TA1, TB1, TK1) :-
	H =.. [P|T],
	map_il_2(T, FA, FB, TA, TB, TK, T1, TA1, TB1, TK1),
	H1 =.. [P|T1].

il_2(T, _, _, TA, TB, TK, T1, TA, TB, TK) :-
	( il_lookup(T, TA, T1) -> true
	; il_lookup(T, TB, T1) -> true
	; il_lookup(T, TK, T1) -> true
	),
	!.
il_2(T, FA, FB, TA, TB, TK, T1, TA1, TB1, TK1) :-
	functor(T, F, N),
	( memberchk(F/N, FA) ->
	  TA1 = [T-T1|TA], TB1 = TB, TK1 = TK,
	  il_gen_var(T1)
	; memberchk(F/N, FB) ->
	  TA1 = TA, TB1 = [T-T1|TB], TK1 = TK,
	  il_gen_var(T1)
	; is_k_term(T) ->
	  TA1 = TA, TB1 = TB, TK1 = [T-T1|TK],
	  T1 = T
	),
	!.
il_2(T, FA, FB, TA, TB, TK, T1, TA1, TB1, TK1) :-
	T =.. [F|T2],
	map_il_2(T2, FA, FB, TA, TB, TK, T3, TA1, TB1, TK1),
	T1 =.. [F|T3].

map_il_2([], _, _, TA, TB, TK, [], TA, TB, TK).
map_il_2([T|T1], FA, FB, TA, TB, TK, [T2|T3], TA1, TB1, TK1) :-
	il_2(T, FA, FB, TA, TB, TK, T2, TA2, TB2, TK2),
	map_il_2(T1, FA, FB, TA2, TB2, TK2, T3, TA1, TB1, TK1).

is_k_term(T) :-
	atom(T),
	atom_prefix(T, '$k').

% is_k_term(T) :-
% 	functor(T, F, _),
% 	atom_prefix(F, '$k').

:- flag(il, _, 1).

il_gen_var(X) :-
	flag(il, N, N+1),
	concat_atom(['$il',N], X).

il_lookup(T, Table, T1) :-
	member(T-T1, Table).

map_il_1([], _, _, TA, TB, TK, [], TA, TB, TK).
map_il_1([H|H1], FA, FB, TA, TB, TK, [H2|H3], TA1, TB1, TK1) :-
	il_1(H, FA, FB, TA, TB, TK, H2, TA2, TB2, TK2),
	map_il_1(H1, FA, FB, TA2, TB2, TK2, H3, TA1, TB1, TK1).

il_logop(true, true, []).
il_logop(false, false, []).
il_logop(~(F), (~), [F]).
il_logop((F,G), (,), [F,G]).
il_logop((F;G), (;), [F,G]).
il_logop((F->G), (->), [F,G]).
il_logop((F<-G), (<-), [F,G]).
il_logop((F<->G), (<->), [F,G]).

wrap_prefix([], F, F).
wrap_prefix([all(X)|Prefix], F, all(X, F1)) :-
	!,
	wrap_prefix(Prefix, F, F1).
wrap_prefix([ex(X)|Prefix], F, ex(X, F1)) :-
	!,
	wrap_prefix(Prefix, F, F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

order_topterms(TA, TB, Options, Prefix) :-
	append(TA, TB, Terms),
	findall((T-X)-(S-Y),
		(member(T-X, Terms),
		  sub_term(S, T),
		  S \= T,
		  memberchk(S-Y, Terms)),
		PGraph1),
	findall('$top'-T1, member(T1, Terms), PGraph2),
	append(PGraph2, PGraph1, PGraph),
	p_to_s_graph(PGraph, SGraph),
	Graph = SGraph,
	from_options(ip_prefer_left=PreferLeft, Options, ex),
	o_tt(Graph, PreferLeft, TA, TB, [], Prefix).

o_tt(Graph, PreferLeft, TA, TB, SoFar, [Q|Prefix]) :-
	( PreferLeft = ex ->
	  ( select(X-Xs, Graph, Graph1),
	    memberchk(X, TA),
	    X=_-V, Q=ex(V)
	  ; select(X-Xs, Graph, Graph1),
	    memberchk(X, TB),
	    X=_-V, Q=all(V)
	  )
	; ( select(X-Xs, Graph, Graph1),
	    memberchk(X, TB),
	    X=_-V, Q=all(V)
	  ; select(X-Xs, Graph, Graph1),
	    memberchk(X, TA),
	    X=_V, Q=ex(X)
	  )
	),
	ord_subset(Xs, SoFar),
	!,
	ord_add_element(SoFar, X, SoFar1),
	o_tt(Graph1, PreferLeft, TA, TB, SoFar1, Prefix).
o_tt(['$top'-_], _, _, _, _, []) :-
	!.
o_tt([], _, _, _, _, []) :-
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).
