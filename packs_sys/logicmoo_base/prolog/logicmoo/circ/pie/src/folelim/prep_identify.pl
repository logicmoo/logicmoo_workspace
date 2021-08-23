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

:- module(prep_identify, [prep_identify/3]).

:- use_module(swilib(hash)).
:- use_module(swilib(info)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Identify atoms that have the same non-recursive positive or negative
%%%% "half definition"
%%%%
%%%% The result is equivalent modulo existential predicate quantification
%%%% on the predicates specified by PredSetSpec
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
prep_identify(M, PredSetSpec, M1) :-
	prep_identify_1(M, PredSetSpec, M1, '$none').

prep_identify_1(M, PSS, M1, LN) :-
	prep_identify_step(M, PSS, M2, LN, NN),
	!,
	prep_identify_1(M2, PSS, M1, NN).
prep_identify_1(M, _, M, _).

prep_identify_step(M, PSS, M1, LN, NN) :-
	m_next_ident(M, PSS, LN, P1, P2, S1, S2, N, NN),
	( S1 = p -> S1I = '' ; S1I = ('~') ),
	( S2 = p -> S2I = '' ; S2I = ('~') ),
	info(20, 'Identifying ~w~q/~q with ~w~q/~q', [S2I,P2,N,S1I,P1,N]),
	m_apply_ident(M, P1, P2, S1, S2, N, M1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_next_ident(M, PredSetSpec, LastNext, P1, P2, S1, S2, N, Next) :-
	m_predicates(M, Ps),
	%% identify roughly bottom up - later generated definitional
	%% predicates have lexically larger names:
	reverse(Ps, Ps1),
	%% increase speed by skipping predicates that did not lead to
	%% identification in the previous round - they are attached at the end
	%% of the list:
	rotate_to(Ps1, LastNext, Ps2),
	findall(P1/N1, ( member(P1/N1, Ps2), ident_candidate(PredSetSpec, P1, N1) ),
		Ps3),
	m_to_ctable(M, MRep),
	mne(Ps3, M, MRep, [], P1, P2, S1, S2, N, Next).

ident_candidate(defpreds, P, _) :-
	!,
	is_def_functor(P).
ident_candidate(keep(Ps), P, N) :-
	!,
	\+ memberchk(P/N, Ps).
ident_candidate(dont_keep(Ps), P, N) :-
	!,
	memberchk(P/N, Ps).

is_def_functor(P) :-
	sub_atom(P, 0, _, _, 'def_'),
	sub_atom(P, _, _, 0, '$').

mne([P/N|Ps], M, MRep, SoFar, P1, P2, S1, S2, N1, Next) :-
	findall(p(P,N,S)-M1, ( mp_has_semidef(M, MRep, P, N, S),
			       mp_canonic_semidef(M, MRep, P, N, S, M1) ),
		This),
	( member(p(P1,N1,S1)-M2, This),
	  member(p(P2,N1,S2)-M3, SoFar),
	  defm_equal(M2, M3) ->
	  ( Ps = [Next|_] -> true ; Next = '$none$' )
	; append(This, SoFar, SoFar1),
	  mne(Ps, M, MRep, SoFar1, P1, P2, S1, S2, N1, Next)
	).

rotate_to(X, '$none$', X) :-
	!.
rotate_to(X, Y, Z) :-
	rotate_to(X, Y, [], Z).

rotate_to([], _, Rest, Ys) :-
	reverse(Rest, Ys).
rotate_to([X|Xs], Y, Rest, Ys) :-
	X == Y,
	!,
	reverse(Rest, Rest1),
	append([X|Xs], Rest1, Ys).
rotate_to([X|Xs], Y, Zs, Us) :-
	rotate_to(Xs, Y, [X|Zs], Us).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

m_to_ctable(M, T) :-
	length(M, L),
	N is L * 4,
	mk_htm(N, T),
	fill_ctable(M, T).

fill_ctable([C|Cs], T) :-
	fct1(C, C, T),
	fill_ctable(Cs, T).
fill_ctable([], _).

fct1([L|Ls], C, T) :-
	( L = ~(A) -> S = n, functor(A, P, N)
	; S = p, functor(L, P, N)
	),
	htm_add(T, p(P,N,S), C),
	fct1(Ls, C, T).
fct1([], _, _).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mp_has_semidef(M, _MRep, P, N, SIGN) :-
	( SIGN = p,
	  \+ ( member(C, M),
	       is_nondef_clause_for_pos(C, P, N) )
	; SIGN = n,
	  \+ ( member(C, M),
	       is_nondef_clause_for_neg(C, P, N) )
	).
	   
is_nondef_clause_for_pos(C, P, N) :-
	member(L, C),
	L \= ~(_),
	functor(L, P, N),
	!,
	( member(~(A), C),
	  functor(A, P, N) ->
	  true
	; member(L2, C),
	  L2 \= ~(_),
	  functor(L2, P, N),
	  L \== L2 ->
	  true
	).
is_nondef_clause_for_neg(C, P, N) :-
	member(~(A), C),
	functor(A, P, N),
	!,
	( member(L, C),
	  L \= ~(_),
	  functor(L, P, N) ->
	  true
	; member(~(A2), C),
	  functor(A2, P, N),
	  A \== A2 ->
	  true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

defm_equal(M1, M2) :-
	mm_rough_match(M1, M2),
	M1 = [[L|_]|_],
	( L = ~(A) ->
	  functor(A, P1, _),
	  S1 = n
	; functor(L, P1, _),
	  S1 = p
	),
	defm_rename_defined(M2, P1, S1, M3),
	M1 =@= M3.

mm_rough_match([[_|C]|Cs], [[_|C1]|Cs1]) :-
	C =@= C1,
	mm_rough_match(Cs, Cs1).
mm_rough_match([], []).

defm_rename_defined(M, P, S, M1) :-
	map_c_drd(M, P, S, M1).
	
map_c_drd([X|Xs], Y1, Y2, [X1|Xs1]) :-
	c_drd(X, Y1, Y2, X1),
	map_c_drd(Xs, Y1, Y2, Xs1).
map_c_drd([], _, _, []).

c_drd([L|C], P, S, [L1|C]) :-
	( L = ~(A) -> A =.. [_|Xs], A1 =.. [P|Xs]
	; L =.. [_|Xs],	 A1 =.. [P|Xs]
	),
	( S = n -> L1 = ~(A1)
	; S = p -> L1 = A1
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lit_sign(~(_), S) :-
	!,
	S =  n.
lit_sign(_, p).

lit_sign_dual(~(_), S) :-
	!,
	S =  p.
lit_sign_dual(_, n).

m_apply_ident(M, P1, P2, S1, S2, N, M1) :-
	map_c_ae(M, P1, P2, S1, S2, N, M1).

map_c_ae([C|Cs], P1, P2, S1, S2, N, M) :-
	member(L, C),
	lit_sign(L, S2),
	lit_functor(L, P2, N),
	!,
	map_c_ae(Cs, P1, P2, S1, S2, N, M).
map_c_ae([C|Cs], P1, P2, S1, S2, N, M) :-
	( map_l_ae(C, P1, P2, S1, S2, N, C1) ->
	  M = [C1|M1]
	; M = M1
	),
	map_c_ae(Cs, P1, P2, S1, S2, N, M1).
map_c_ae([], _, _, _, _, _, []).


map_l_ae(C, P1, P2, S1, S2, N, C1) :-
	map_l_ae_1(C, P1, P2, S1, S2, N, ModP, C2),
	( ModP == modified ->
	  sort(C2, C1),
	  ( c_is_tautologic(C1) -> writeln(taut), fail ; true )
	; C1 = C2
	).

%%%%
%%%% S1=S2=p    replace ~p2 with ~p1
%%%% S1=S2=n    replace +p2 with +p1
%%%% S1=p S2=n  replace +p2 with ~p1
%%%% S1=n S2=p  replace ~p2 with +p1
%%%%

map_l_ae_1([L|Ls], P1, P2, S1, S2, N, ModP, [L1|Ls1]) :-
	l_ae(L, P1, P2, S1, S2, N, ModP, L1),
	map_l_ae_1(Ls, P1, P2, S1, S2, N, ModP, Ls1).
map_l_ae_1([], _, _, _, _, _, _, []).

l_ae(L, P1, P2, S1, S2, N, ModP, L1) :-
	lit_sign_dual(L, S2),
	lit_functor(L, P2, N),
	!,
	ModP = modified,
	( L = ~(A) ->  %% i.e. S2=p
	  A =.. [_|X],
	  A1 =.. [P1|X],
	  ( S1 = p -> L1 = ~(A1)
	  ; S1 = n -> L1 = A1
	  )
	; L =.. [_|X], %% i.e. S2=n
	  A1 =.. [P1|X],
	  ( S1 = p -> L1 = ~(A1)
	  ; S1 = n -> L1 = A1
	  )
	).
l_ae(L, _, _, _, _, _, _, L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mp_canonic_semidef(_M, MRep, P, N, SIGN, M1) :-
	findall(C0, htm_get(MRep, p(P,N,SIGN), C0), M0),
	( SIGN = p ->
	  findall(C, ( member(C, M0),
		       once((member(L, C), L \= ~(_), functor(L, P, N))) ),
		  M2)
	; SIGN = n ->
	  findall(C, ( member(C, M0),
		       once((member(~(A), C), functor(A, P, N))) ),
		  M2)
	),
	defm_canonic(M2, P, N, M1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lit_functor(~A, F, N) :-
	!,
	functor(A, F, N).
lit_functor(A, F, N) :-
	functor(A, F, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

defm_canonic(M, P, N, M1) :-
	map_defc_canonic(M, P, N, M2),
	keysort(M2, M3),
	map_val_1(M3, M1).

defc_canonic(C, P, N, [L1|C1]) :-
	%% should be canonic if all vars in C are in the
	%% one literal with predicate P/N
	select(L, C, C2),
	lit_functor(L, P, N),
	!,
	copy_term(L-C2, L1-C3),
	sort(C3, C1).

map_defc_canonic([X|Xs], Y1, Y2, [K-X1|Xs1]) :-
	defc_canonic(X, Y1, Y2, X1),
	copy_term(X1, K),
	numbervars(K, 0, _),
	map_defc_canonic(Xs, Y1, Y2, Xs1).
map_defc_canonic([], _, _, []).

map_val_1([_-X|Xs], [X|Xs1]) :-
	map_val_1(Xs, Xs1).
map_val_1([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


				% lcld(X), dps(Y), member(PN1, Y), member(PN2, Y),  PN1 @< PN2, m_occs(PN1, X, PP1, NN1), m_occs(PN2, X, PP2, NN2), m_eq(PP1