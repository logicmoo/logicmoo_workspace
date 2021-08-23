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

:- module( preprocexp,
	   [ m_red_condense_limited/3,
	     m_red_condense/2,
	     m_add_pullout_equality/2,
	     set_split_pred_counter/1,
	     m_split/2,
	     m_flatten/2,
	     m_red_subsres/2] ).

:- use_module(nf(nf), [clause_sort_lits_by_pred_occurrences/2,
		       clause_subsumes_chk/2]).
:- use_module(swilib(options)).
:- use_module(swilib(info)).
:- use_module(swilib(err)).
:- use_module(swilib(sysdep)).

:- (prolog_flag(dialect, yap) -> use_module(swilib(yap_support)) ; true ).

% :- use_module(prooftask_cm, [inf_limit_factor/1]).
inf_limit_factor(10000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% CLAUSE COMPONENT SPLITTING
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- flag(split_pred_counter, _, 0).

set_split_pred_counter(N) :-
	flag(split_pred_counter, _, N).	

gen_split_pred(P) :-
	flag_inc(split_pred_counter, N),
	concat_atom(['$s', N], P).

m_split(M, M1) :-
	flag(split_pred_counter, S1, S1),
	map_m_split(M, M1),
	flag(split_pred_counter, S2, S2),
	S is S2 - S1,
	info(20, 'Introduced ~w split predicates', [S]).

c_class(Ls, Class) :-
	c_class(Ls, negative, Class).
c_class([~_|Ls], N, N1) :-
	!,
	c_class(Ls, N, N1).
c_class([_|Ls], N, N1) :-
	( N = definite ->
	  N1 = nonhorn
	; c_class(Ls, definite, N1)
	).
c_class([], N, N).

map_m_split([C|Cs], Cs1) :-
	split_clause(C, Ps, GP),
	( Ps = [_] -> Cs1 = [C|Cs2]
	; mk_splits(Ps, K, Cs3),
	  append(GP, K, CP),
	  append([CP|Cs3], Cs2, Cs1)
	),
	map_m_split(Cs, Cs2).
map_m_split([], []).

mk_splits([K|Ks], [L1|Ls], [[L2|K]|Cs]) :-
	gen_split_pred(A),
	c_class(K, Class),
	( Class = definite ->  
	  L1 = A, L2 = ~A
	; L1 = ~A, L2 = A
	),
	mk_splits(Ks, Ls, Cs).
mk_splits([], [], []).		 


check_split(C, P, G) :-
	sort(C, C1),
	apply_append([G|P], C2),
	sort(C2, C3),
	( C1 \== C3 ->
	  err('Check split: ~q', [C])
	; true
	).

apply_append([L|Ls], L1) :-
	append(L, L2, L1),
	apply_append(Ls, L2).
apply_append([], []).

split_clause(C, Partitions, GroundPartition) :-
	spcl(C, Classes),
	init_partitions(Classes, Partitions1),
	map_spcl_1(C, Classes, Partitions1, Partitions, GroundPartition).
%	check_split(C, Partitions, GroundPartition).

init_partitions([_|Cs], [[]|Ps]) :-
	init_partitions(Cs, Ps).
init_partitions([], []).

map_spcl_1([L|Ls], Classes, Partitions, Partitions1, GP) :-
	term_variables(L, Vs),
	( Vs = [] ->
	  GP = [L|GP1],
	  Partitions2 = Partitions
	; sort(Vs, Vs1),
	  map_spcl_2(Vs1, L, Classes, Partitions, Partitions2),
	  GP = GP1
	),
	map_spcl_1(Ls, Classes, Partitions2, Partitions1, GP1).
map_spcl_1([], _, Partitions, Partitions, []).

map_spcl_2(Vs, L, [C|_], [P|Ps], [[L|P]|Ps]) :-
	ord_subset(Vs, C),
	!.
map_spcl_2(Vs, L, [_|Cs], [P|Ps], [P|Ps1]) :-
	!,
	map_spcl_2(Vs, L, Cs, Ps, Ps1).
map_spcl_2(_, L, Cs, _, _) :-
	err('Partitioning failure for ~q in ~q', [L, Cs]).

spcl(C, Classes) :-
	spcl(C, [], Classes).

spcl([L|Ls], Classes, Classes1) :-
	term_variables(L, Vs),
	( Vs = [] ->
	  Classes2 = Classes
	; sort(Vs, Vs1),
	  mrg_classes(Classes, Vs1, Vs1, Classes2)
	),
	spcl(Ls, Classes2, Classes1).
spcl([], Classes, Classes).

mrg_classes([C|Cs], C0, Vs, Cs1) :-
	ord_intersect(Vs, C),
	!,
	ord_union(C, C0, C1),
	mrg_classes(Cs, C1, Vs, Cs1).
mrg_classes([C|Cs], C0, Vs, [C|Cs1]) :-
	mrg_classes(Cs, C0, Vs, Cs1).
mrg_classes([], C0, _, [C0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Condensation
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_red_condense(M, M1) :-
	map_c_condense_limited(M, -1, M1).

%%%% 
%%%% Apply fol_sort_lits_by_pred_occurrences before
%%%% 
m_red_condense_limited([], _, []) :-
	!.
m_red_condense_limited(M, Limit, M1) :-
	length(M, Len),
	inf_limit_factor(InfFactor),
	Limit1 is round((Limit / Len)*InfFactor),
	map_c_condense_limited(M, Limit1, M1).

map_c_condense_limited([X|Xs], Limit, [X1|Xs1]) :-
	( Limit =< 0 ->
	  c_condense_unlimited(X, X1)
	; c_condense_limited(X, Limit, X1)
	),
%	( X \== X1 -> writeln('CONDENSATION') ; true ),
	map_c_condense_limited(Xs, Limit, Xs1).
map_c_condense_limited([], _, []).

c_condense_unlimited(C, C1) :-
	length(C, L1),
	c_enum_factor(C, C2),
	length(C2, L2),
	L2 < L1,
	clause_sort_lits_by_pred_occurrences(C2, C3),
	clause_subsumes_chk(C3, C),
	!,
	c_condense_unlimited(C3, C1).
c_condense_unlimited(C, C).

c_condense_limited(C, Limit, C1) :-
	length(C, L1),
	call_with_inference_limit(( c_enum_factor(C, C2),
				    length(C2, L2),
				    L2 < L1,
				    clause_sort_lits_by_pred_occurrences(C2,
									 C3),
				    clause_subsumes_chk(C3, C)
				  ),
				  Limit,
				  Result),
	( Result == true ->
	  !
	; Result == ! ->
	  !
	; fail
	),
	c_condense_limited(C3, Limit, C1).
c_condense_limited(C, _, C).


%%
%% An example: c_enum_factor([p(U,U), p(X,Y), p(A,B), q(X,Y,A,B,U)], F).
%%
c_enum_factor(C, C1) :-
	copy_term(C, C2),
	c_enum_factor_nocopy(C2, C1).
		  
c_enum_factor_nocopy(C, C1) :-
	c_enum_factor_1(C, C2),
	sort(C2, C1),
	\+ c_is_tautologic(C1).

c_enum_factor_1([L|C], [L|C1]) :-	
	c_enum_factor_nocopy(C, C1),
	( true
	; member(L1, C1),
	  unify_with_occurs_check(L, L1)
	).
c_enum_factor_1([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% PULLOUT (ELIMINATION OF MONOTONICITY)
%%%%
%%%% i.e. flattening, based on old code in nf/brand.pl
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_flatten(M, M1) :-
	matrix_pullout(M, [], M1).

%%%% 
%%%% 
%%%% 
m_add_pullout_equality(M, M1) :-
	matrix_pullout(M, [], M2),
	equality_rst_axioms_r_st(M3),
	append(M3, M2, M1).

matrix_pullout(M, Excepts, M1) :-
	map_pullout_clause(M, Excepts, M1).

map_pullout_clause([C|Cs], Excepts, [C1|Cs1]) :-
	pullout_clause(C, Excepts, C1),
	map_pullout_clause(Cs, Excepts, Cs1).
map_pullout_clause([], _, []).

pullout_clause(C, Excepts, C1) :-
	pullout_lits(C, Excepts, C2),
	copy_term(C2, C3),
	pout_simplify(C3, C1).


pullout_lits([~(X=Y)|Ls], Excepts, C1) :-
	\+ var(Y),
	!,
	( var(X) ->
	  pullout_lits([~(Y=X)|Ls], Excepts, C1)
	; pullout_lits([~(Y=Z),~(X=Z)|Ls], Excepts, C1)
	).
pullout_lits([X=Y|Ls], Excepts, C1) :-
	\+ var(Y),
	!,
	( var(X) ->
	  pullout_lits([Y=X|Ls], Excepts, C1)
	; pullout_lits([Y=Z,~(X=Z)|Ls], Excepts, C1)
	).
pullout_lits([L|Ls], Excepts, C1) :-
	Options = [],                       %% Options is currently not used
	pullout_literal(L, Excepts, Options, Ls1),
	append(Ls1, C2, C1),
	pullout_lits(Ls, Excepts, C2).
pullout_lits([], _, []).

pullout_literal(Literal, Excepts, Options, Literals) :-
	( Literal = ~Atom ->
	  Literal1 = ~Atom1
        ; Atom = Literal,
	  Atom1 = Literal1
        ),
	functor(Atom, O, A),
	functor(Atom1, O, A),
	( Atom = (_ = _) ->
	  map_pout_subterms(A, Atom, Atom1, Excepts, Options, [], Equations)
        ; map_pout_a(A, Atom, Atom1, Excepts, Options, [], Equations)
        ),
	append(Equations, [Literal1], Literals).

map_pout_subterms(0, _, _, _, _, Eqs, Eqs) :- !.
map_pout_subterms(A, F, F1, Excepts, Opt, Eqs, Eqs1) :-
	arg(A, F, G),
	arg(A, F1, G1),
	pout_subterms(G, G1, Excepts, Opt, Eqs, Eqs2),
	A1 is A - 1,
	map_pout_subterms(A1, F, F1, Excepts, Opt, Eqs2, Eqs1).

pout_subterms(T, T, _, _, Eqs, Eqs) :-
	var(T),
	!.
pout_subterms(F, F1, Excepts, Opt, Eqs, Eqs1) :-
	functor(F, O, A),
	functor(F1, O, A),
	map_pout_a(A, F, F1, Excepts, Opt, Eqs, Eqs1).

map_pout_a(0, _, _, _, _, Eqs, Eqs) :- !.
map_pout_a(A, F, F1, Excepts, Opt, Eqs, Eqs1) :-
	arg(A, F, G),
	arg(A, F1, G1),
	pout_a(G, G1, Excepts, Opt, Eqs, Eqs2),
	A1 is A - 1,
	map_pout_a(A1, F, F1, Excepts, Opt, Eqs2, Eqs1).

pout_a(T, T, _Excepts, _, Eqs, Eqs) :-
	var(T),
	!.
pout_a(T, T, Excepts, _, Eqs, Eqs) :-
	functor(T, F, N),
	memberchk(F/N, Excepts),
	!.
pout_a(F, X, Excepts, Opt, Eqs, Eqs1) :-
	functor(F, O, A),
	functor(F1, O, A),
	map_pout_a(A, F, F1, Excepts, Opt, [~(F1 = X)|Eqs], Eqs1).

pout_simplify([~(U = V)|Eqs], Eqs1) :-            %% Apply Reflexivity
	var(U),
	var(V),
	!,
	U = V,
	pout_simplify(Eqs, Eqs1).
pout_simplify([~(T = V)|Eqs], [~(T = V)|Eqs1]) :- %% Merge equations with
	                                          %% identical left sides
	var(V),
	select(~(T1 = V1), Eqs, Eqs2),
	var(V1),
	T == T1,
	!,
	V = V1,
	pout_simplify(Eqs2, Eqs1).
pout_simplify([Eq|Eqs], [Eq|Eqs1]) :-
	pout_simplify(Eqs, Eqs1).
pout_simplify([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% lit_complement(~X,X) :- !.
% lit_complement(X,~X).

lit_atom(~X,X) :- !.
lit_atom(X,X).	      


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

binres(M, Options, M1) :-
	findall(C, ( member(C, M), C = [_,_] ), Bins),
	findall(U, member([U], M), Units),
	br_loop_1(Units, Bins, ['$*'|Units], UsResult),
	reverse(UsResult, Units2),
	once(memrest('$*', Units2, Units3)),
	( from_options( binres_units=pos, Options, all ) ->
	  findall(U, ( member(U, Units3), U \= ~(_) ), Units4)
	; Units4 = Units3
	),
	from_options( binres_add_query=AddQuery, Options, none ),
	( AddQuery=all ->
	  findall(['$query', U], member(U, Units4), Units5)
	; AddQuery=neg ->
	  findall(C, ( member(U, Units4),
		       (U = ~_ -> C = ['$query',U] ; C = [U] )
		     ),
		  Units5)
	; findall([U], member(U, Units4), Units5)
	),
	append(Units5, M, M1).

debug_check_binres(M, M1) :-
	member(C, M),
	\+ ( member(D, M1), C =@= D ),
	!,
	err('Binres error: ~q missing', [D]).
debug_check_binres(_, _).

memrest(X, [X|Xs], Xs).
memrest(X, [_|Xs], Ys) :-
	memrest(X, Xs, Ys).


br_subsumed(X, Xs, _) :-
	member(A, Xs),
	subsumes_chk(A, X),
	!.
br_subsumed(X, _, Xs) :-
	member(A, Xs),
	subsumes_chk(A, X),
	!.

br_loop_1([], _, Us, Us) :-
	!.
br_loop_1(UsTP, Bs, OldUs, UsResult) :-
	br1(UsTP, Bs, Bs, OldUs, [], NewUs),
	append(NewUs, OldUs, OldUs1), 
	br_loop_1(NewUs, Bs, OldUs1, UsResult).

br1([U|Us], Bins, AllBins, OldUs, NewUs, NewUs1) :-
	copy_term(U, U1),
	memrest(AB, Bins, Bins1),
	copy_term(AB, [A,B]),
	lit_complem(U1, U2),
	( unify_with_occurs_check(U2, A),
	  \+ br_subsumed(B, NewUs, OldUs) ->
	  NewUs2 = [B|NewUs]
	; unify_with_occurs_check(U2, B),
	  \+ br_subsumed(A, NewUs, OldUs) ->
	  NewUs2 = [A|NewUs]
	),
	!,
	br1([U|Us], Bins1, AllBins, OldUs, NewUs2, NewUs1).
br1([_|Us], _, AllBins, OldUs, NewUs, NewUs1) :-
	br1(Us, AllBins, AllBins, OldUs, NewUs, NewUs1).
br1([], _, _, _, NewUs, NewUs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lit_complem(~A, A) :-
	!.
lit_complem(A, ~A).

c_is_tautologic(C) :-
	member(~A, C),
	member(B, C),
	A == B,
	!.
c_is_tautologic(C) :-
	member((X = Y), C),
	X == Y,
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Subsumption Resolution, Naive Implementation
%%%% 
m_red_subsres(M, M1) :-
	m_rs_step(M, M2),
	!,
	m_red_subsres(M2, M1).
m_red_subsres(M, M).
	

select2(X, [X|Y], [], Y).
select2(X, [Y|Z], [Y|U], V) :-
	select2(X, Z, U, V).


m_rs_step(M, M1) :-
	select2(C1, M, M2, M3),
	copy_term(C1, C1C),
	select(L1, C1C, C1R),
	lit_complem(L1, NL1),
	member(C2, M),
	\+ \+ ( member(X, C2), unify_with_occurs_check(NL1, X) ),
	copy_term(C2, C2C),
	select(L2, C2C, C2R),
	unify_with_occurs_check(NL1, L2),
	append(C1R, C2R, R1),
	sort(R1, R2),
	\+ c_is_tautologic(R2),
	c_subsumes_chk(R2, C1),
	\+ c_subsumes_chk(C1, R2),
	!,
	append(M2, [R2|M3], M1).
		   
c_subsumes_chk(C1, C2) :-
	\+ \+ ( copy_term(C2, C2c),
		term_variables(C2c, Vs),
		copy_term(Vs, Vpattern),
		c_subsumes1_chk(C1, C2c, Vs, Vpattern)
	      ).
	
c_subsumes1_chk([],_,_,_).
c_subsumes1_chk([L|Ls], C2, Vs, Vpattern) :-
	member(L1, C2),
	unify_with_occurs_check(L, L1),
	subsumes_term(Vs, Vpattern),
	c_subsumes1_chk(Ls, C2, Vs, Vpattern).
