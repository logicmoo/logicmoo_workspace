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


:- module(prep_resol,
	  [m_red_elim_cheap/3,
	   m_red_purity/3]).

:- use_module(swilib(info)).

% 0*N -> remove clauses with the predicate
% 1*1 -> rbr
% 1*M and no multiple occurrences in the M -> rbr
% N*M and no multiple occurrences in M, N and less nontaut clauses -> rbr

is_protected(FN, Protected) :-
	memberchk(FN, Protected),
	!.
is_protected(F/_, _) :-
	sub_atom(F, 0, _, _, '$').

m_red_elim_cheap(M, Protected, M1) :-
	m_red_elim_cheap_step(M, Protected, M2),
	!,
	m_red_elim_cheap(M2, Protected, M1).
m_red_elim_cheap(M, _, M).

m_red_elim_cheap_step(M, Protected, M1) :-
	m_pred_occs(M, Occs),
	occs_pn(Occs, Ps, Ns),
	occs_mrg_for_elim(Ps, Ns, Protected, Es),
	sort(Es, Es1),
	( Es1 = [ekey(0, _)-E|_] ->
	  elim_cheap_zero(M, E, M1)
	; Es1 = [ekey(1, _)-E|_] ->
	  elim_cheap_one(M, E, M1)
	; Es = [_-E|_] ->
	  elim_cheap_times(M, E, [], M1)
	; fail
	).

elim_cheap_times(M, e(F,A,N1,N2), Options, M1) :-
	check_elim_cheap_times_pre(N1, N2, Options),
	functor(T, F, A),
	T1 = T,
	T2 = ~T,
	select_clauses_with_template(M, T1, Cs1, M3),
	select_clauses_with_template(M3, T2, Cs2, M4),
	bin_resolvents(T1, T2, Cs1, Cs2, Res),
	length(Res, N3),
	check_elim_cheap_times_post(N1, N2, N3, Options),
	N4 is N3 - (N2 + N1),
	( N4 =< 0 ->
	  info(50, 'Cheaply eliminating ~q (not more clauses)', [F/A])
	; info(50, 'Cheaply eliminating ~q (~w more clauses)', [F/A, N4])
	),
	append(Res, M4, M1).

check_elim_cheap_times_pre(N1, N2, _) :-
	N1*N2 < 10.

check_elim_cheap_times_post(N1, N2, N3, _) :-
	N3 < N1+N2,
	!.
check_elim_cheap_times_post(N1, N2, N3, _) :-
	N3 < N1+N2+4.

elim_cheap_zero(M, e(F,A,NP,NN), M1) :-
	( NP = 0 ->
	  Pol = neg
	; NN = 0 ->
	  Pol = pos
	),
	info(50, 'Cheaply eliminating ~q (purity: only ~w)', [F/A, Pol]),
	m_rm_clauses_with(M, [F/A], M1).
	
%%%% 
%%%% For the cases: NP=1 or NN=1 (single pos or single neg occurrence)
%%%% 
elim_cheap_one(M, e(F,A,NP,NN), M1) :-
	functor(T, F, A),
	( NP = 1 ->
	  T1 = T,
	  T2 = ~T,
	  Pol = pos
	; NN = 1 ->
	  T1 = ~T,
	  T2 = T,
	  Pol = neg
	),
	info(50, 'Cheaply eliminating ~q (single ~w occurrence)', [F/A, Pol]),
	once(( select(C, M, M2), \+ \+ memberchk(T1, C) )),
	select_clauses_with_template(M2, T2, Cs, M3),
	bin_resolvents(T1, T2, [C], Cs, Cs1),
	append(Cs1, M3, M1).

bin_resolvents(T1,T2, Cs1, Cs2, Rs) :-
	findall(R, bin_resolvent(T1, T2, Cs1, Cs2, R), Rs).

bin_resolvent(T1, T2, Cs1, Cs2, R) :-
	member(C1, Cs1),
	copy_term(C1, D1),
	once(select(T1, D1, E1)),
	member(C2, Cs2),
	copy_term(C2, D2),
	once(select(T2, D2, E2)),
	append(E1, E2, R1),
	unify_with_complement(T1, T2),
	sort(R1, R),
	\+ c_is_tautologic(R).

unify_with_complement(~A, B) :-
	!,
	unify_with_occurs_check(A, B).
unify_with_complement(A, ~B) :-
	unify_with_occurs_check(A, B).

select_clauses_with_template([C|Cs], T, [C|Cs1], M) :-
	\+ \+ memberchk(T, C),
	!,
	select_clauses_with_template(Cs, T, Cs1, M).
select_clauses_with_template([C|Cs], T, Cs1, [C|M]) :-
	select_clauses_with_template(Cs, T, Cs1, M).
select_clauses_with_template([], _, [], []).

occs_mrg_for_elim([p(p,F,A,N1,O1)|Xs], [p(n,F,A,N2,O2)|Ys], Prot, Ps) :-
	!,
	( %% no protected predicates
	  is_protected(F/A, Prot) ->
	  Ps = Ps1
	; %% no predicates with multiple occurrences in a clause
	  O1+O2 > 0 ->
	  Ps = Ps1
	; Min is min(N1,N2),
	  Product is N1*N2,
	  K = ekey(Min,Product),
	  Ps = [K-e(F,A,N1,N2)|Ps1]
	),
	occs_mrg_for_elim(Xs, Ys, Prot, Ps1).
occs_mrg_for_elim([X|Xs], [Y|Ys], Prot, Ps) :-
	!,
	X = p(p,F1,A1,N1,_),
	Y = p(n,F2,A2,N2,_),
	( F1/A1 @< F2/A2 ->
	  ( is_protected(F1/A1, Prot) ->
	    Ps = Ps1
	  ; Ps = [K-E|Ps1],
	    K = ekey(0,0),
	    E = e(F1,A1,N1,0)
	  ),
	  occs_mrg_for_elim(Xs, [Y|Ys], Prot, Ps1)
	; ( is_protected(F2/A2, Prot) ->
	    Ps =Ps1
	  ; Ps = [K-E|Ps1],
	    K = ekey(0,0),
	    E = e(F2,A2,0,N2)
	  ),
	  occs_mrg_for_elim([X|Xs], Ys, Prot, Ps1)
	).
occs_mrg_for_elim([X|Xs], [], Prot, Ps) :-
	X = p(p,F,A,N,_),
	( is_protected(F/A, Prot) ->
	  occs_mrg_for_elim(Xs, [], Prot, Ps)
	; Ps = [K-E],
	  K = ekey(0,0),
	  E = e(F,A,N,0)
	).
occs_mrg_for_elim([], [X|Xs], Prot, Ps) :-
	X = p(n,F,A,N,_),
	( is_protected(F/A, Prot) ->
	  occs_mrg_for_elim([], Xs, Prot, Ps)
	; Ps = [K-E],
	  K = ekey(0,0),
	  E = e(F,A,0,N)
	).
occs_mrg_for_elim([], [], _, []).


m_red_purity(M, Protected, M1) :-
	m_pred_occs(M, Occs),
	occs_pures(Occs, Protected, Pure),
	( Pure = [] ->
	  M = M1
	; info(50, 'Cheaply eliminating ~q (purity)', [Pure]),
	  m_rm_clauses_with(M, Pure, M1)
	).

occs_pn([p(n,F,A,N,O)|Xs], Ys, [p(n,F,A,N,O)|Xs1]) :-
	!,
	occs_pn(Xs, Ys, Xs1).
occs_pn(Xs, Xs, []).

occs_pures(Occs, Protected, Pure) :-
	occs_pn(Occs, Ps, Ns),
	occs_mrg_pure(Ps, Ns, Protected, Pure).

occs_mrg_pure([p(p,F,A,_,_)|Xs], [p(n,F,A,_,_)|Ys], Prot, Pure) :-
	!,
	occs_mrg_pure(Xs, Ys, Prot, Pure).
occs_mrg_pure([X|Xs], [Y|Ys], Prot, Pure) :-
	X = p(p,F1,A1,_,_),
	Y = p(n,F2,A2,_,_),
	( F1/A1 @< F2/A2 ->
	  ( is_protected(F1/A1, Prot) ->
	    Pure = Pure1
	  ; Pure = [F1/A1|Pure1]
	  ),
	  occs_mrg_pure(Xs, [Y|Ys], Prot, Pure1)
	; ( is_protected(F2/A2, Prot) ->
	    Pure = Pure1
	  ; Pure = [F2/A2|Pure1]
	  ),
	  occs_mrg_pure([X|Xs], Ys, Prot, Pure1)
	).
occs_mrg_pure(Xs, [], Prot, Pure) :-
	!,
	findall(F/A, ( member(p(p,F,A,_,_), Xs),
		       \+ is_protected(F/A, Prot) ),
		Pure).
occs_mrg_pure([], Xs, Prot, Pure) :-
	!,
	findall(F/A, ( member(p(n,F,A,_,_), Xs),
		       \+ is_protected(F/A, Prot) ),
		Pure).

m_rm_clauses_with([C|M], Pure, M1) :-
	member(L, C),
	lit_functor(L, F, N),
	memberchk(F/N, Pure),
	!,
	m_rm_clauses_with(M, Pure, M1).
m_rm_clauses_with([C|M], Pure, [C|M1]) :-
	m_rm_clauses_with(M, Pure, M1).
m_rm_clauses_with([], _, []).
	
lit_functor(~A, F, N) :- !, functor(A, F, N).
lit_functor(A, F, N) :- functor(A, F, N).
	
m_pred_occs(M, Occs) :-
	pro_1(M, [], Ps1),
	msort(Ps1, Ps2),
	( Ps2 = [X|Xs] ->
	  pro_3(Xs, X, 1, Occs1)
	; Occs1 = []
	),
	sort(Occs1, Occs).

pro_3([X|Xs], X, N, Ys) :-
	!,
	N1 is N+1,
	pro_3(Xs, X, N1, Ys).
pro_3([Y|Xs], p(S,F,A,MultOcc), N, [p(S,F,A,N,MultOcc1)|Ys]) :-
	( MultOcc == true ->
	  MultOcc1 = 1
	; MultOcc1 = 0
	),
	pro_3(Xs, Y, 1, Ys).
pro_3([], p(S,F,A,MultOcc), N, [p(S,F,A,N,MultOcc1)]) :-
	( MultOcc == true ->
	  MultOcc1 = 1
	; MultOcc1 = 0
	).


pro_1([C|Cs], P, P1) :-
	pro_2(C, P, P2),
	pro_1(Cs, P2, P1).
pro_1([], P, P).

pro_2([~A|Ls], P, P1) :-
	!,
	functor(A, F, N),
	p_in_rest(Ls, F, N, Occ),
	pro_2(Ls, [p(n,F,N,Occ)|P], P1).
pro_2([A|Ls], P, P1) :-
	functor(A, F, N),
	p_in_rest(Ls, F, N, Occ),
	pro_2(Ls, [p(p,F,N,Occ)|P], P1).
pro_2([], P, P).

p_in_rest([~A|Ls], P, N, Occ) :-
	!,
	( functor(A, P, N) ->
	  Occ = true
	; p_in_rest(Ls, P, N, Occ)
	).
p_in_rest([A|Ls], P, N, Occ) :-
	( functor(A, P, N) ->
	  Occ = true
	; p_in_rest(Ls, P, N, Occ)
	).
p_in_rest([], _, _, _).


c_is_tautologic(C) :-
	member(~A, C),
	member(B, C),
	A == B,
	!.
c_is_tautologic(C) :-
	member((X = Y), C),
	X == Y,
	!.
	