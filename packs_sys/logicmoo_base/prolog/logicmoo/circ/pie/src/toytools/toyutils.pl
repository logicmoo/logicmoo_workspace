%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2010, 2015 Christoph Wernhard
%%%%
%%%% This program is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%% 
%%%% This program is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%% 
%%%% You should have received a copy of the GNU General Public License
%%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(toyutils, [kb/1,
		     kb/2,
		     factbase/1,
		     factbase/2,
		     print_signature/0,
		     assert_factbase_in_prolog/0,
		     simp_nf/2,
		     dnf_simp/2,
		     cnf_simp/2,
		     dnf_prime/2,
		     cnf_prime/2,
		     
		     ebase/2,
		     is_valid/1,
		     is_satisfiable/1,
		     is_unsatisfiable/1,
		     is_extending_conservatively/2,
		     is_extending_conservatively/3,
		     is_definable/3,
		     detect_definability/3,
		     extract_definition/4]).
		     
:- use_module(nf(nf)).
:- use_module(nf(nfutils)).
:- use_module(swilib(pretty)).
:- use_module(toyproject).
:- use_module(grounder).
:- use_module(auxiliary).

:- op(1200, xfx, user:'::').
:- multifile(user:'::'/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Tools for Debugging KBs
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%% 
%%%% F is the full KB as a first-order sentence. Useful in connection
%%%% with other predicates that operate on first-order sentences.
%%%% 
kb(Spec, F) :-
	findall(G, ((Label :: G), (memberchk(Label, Spec))), Gs),
	list_to_conjunction(Gs, F).

factbase(Spec, F) :-
	findall(G, ((Label :: G),
		    Label \= rules(_),
		   (memberchk(Label, Spec))), Gs),
	list_to_conjunction(Gs, F).

kb(F) :-
	kb([_], F).

factbase(F) :-
	factbase([_], F).


%%%%
%%%% Consider the whole KB as a first-order sentence and print out
%%%% its predicates and functions. Useful to find typos.
%%%% 
print_signature :-
	kb(X),
	f_signature(X, Ps, Fs),
        writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
	writeln('%%%% Predicates:'),
	writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
	( member(P, Ps),
	  writeq(P),
	  nl,
	  fail
	; true
	),
	writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
	writeln('%%%% Functions:'),
	writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
	( member(F, Fs),
	  writeq(F),
	  nl,
	  fail
	; true
	).


%%%% 
%%%% Assert all syntactic ground facts of the KB as Prolog facts.
%%%% Useful for debugging or playing around.
%%%% 
assert_factbase_in_prolog :-
	factbase(F),
	conjunction_to_list(F, Fs),
	( member(G, Fs),
	  cnf(G, G1),
	  ( G1 = [[G2]], ground(G2), \+ catch(G2, _, fail) ->
	    assert(G2)
	  ; true
	  ),
	  fail
	; true
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Computing Satisfiability etc. in Terms of Uniform Interpolants
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_satisfiable(F) :-
	elim(proj([], F), X),
	X = true.

is_unsatisfiable(F) :-
	elim(proj([], F), X),
	X = false.

is_valid(F) :-
	elim(proj([], ~F), X),
	X = false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Computing the Essential Literal Base
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ebase(F, S) :-
	xbase(F, S1),
	remove_inessential_lits(S1, F, S).

remove_inessential_lits([L|Ls], F, Ls1) :-
	is_valid( valid_iff_not_in_ebase(L, F) ),
	!,
	remove_inessential_lits(Ls, F, Ls1).
remove_inessential_lits([L|Ls], F, [L|Ls1]) :-
	remove_inessential_lits(Ls, F, Ls1).
remove_inessential_lits([], _, []).

:- define_elim_macro(valid_iff_not_in_ebase(L, F),
		     (forg([L], F) -> F)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Globally Weakest Sufficient and Strongest Necessary Condition
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- define_elim_macro(gwsc(S, F, G), ~proj(complements(S), (F, ~G))).
:- define_elim_macro(gsnc(S, F, G), proj(S, (F, G))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Definability
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- define_elim_macro(valid_iff_definable(S, F, G),
		     (gsnc(S, F, G) -> gwsc(S, F, G))).

%%%% 
%%%% valid_iff_definable(S, F, G) is valid iff
%%%% there is a sentence G' in S such that
%%%% F |= (G' <-> G)
%%%% i.e. G is definable in terms of S within F
%%%% 
%%%% If G is definable in terms of S within F, then
%%%% both, snc(S, F, G) and wnc(S, F, G) are "definitions"
%%%% of G in S within F. That is, it hold that:
%%%% F |= (gwsc(S, F, G) <-> G), and
%%%% F |= (gsnc(S, F, G) <-> G)
%%%% 

is_definable(S, F, G) :-
	is_valid( valid_iff_definable(S, F, G) ).

extract_definition(S, F, G, D) :-
	( is_definable(S, F, G) ->
	  ( elim(gsnc(S, F, G), D)
	  ; elim(gwsc(S, F, G), D)
	  )
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% KB Modularization - Conservative Extension
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- define_elim_macro(valid_iff_extending_conservatively(S, F, G),
		     (F -> snc(S, F, G))).

is_extending_conservatively(S, F, G) :-
	is_valid( valid_iff_extending_conservatively(S, F, G) ).

is_extending_conservatively(F, G) :-
	xabase(F, S),
	is_valid( valid_iff_extending_conservatively(S, F, G) ).

%%%% 
%%%% Typically
%%%% - F is the original ontology
%%%% - G is added (the conjunction F&G is the extension)
%%%% - S is the atom base of F
%%%%   (the essential atom base for an entirely semantic characterization)
%%%% 
%%%% If S is the base of F, then the converse of the implication
%%%% "F -> proj(S, (F, G))" does always hold
%%%% 
%%%% - If S' >= S and (F -> proj(S', F, G)) then (F -> proj(S, F, G))
%%%%
%%%%   So a scope S' that is a superset of F's essential atom base
%%%%   preserves the "is_extending_conservatively/2" property.
%%%%   Hence the use of xabase does not affect correctness of
%%%%   true "is_extending_conservatively/2" statements.
%%%%
%%%% - S can also be another scope, about which no new info
%%%%   may be inferred through adding G (data protetion,
%%%%   e.g. S = [credit_card_number(_,_)]
%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% Enumerate all P, S such that predicate P is definable in terms of
%%%% predicates S. Naive implementation, suitable just for small examples.
%%%% 
detect_definability(F, P, S) :-
	xabase(F, Base),
	findall(A, member(+A, Base), ABase),
	select(P, ABase, Rest),
	minimal_subset(Rest, is_definable(_, F, P), S).

%%%% 
%%%% minimal_subset(Set, Test, X)
%%%% 
%%%%    - Set is a list, representing a set
%%%%    - Test is a pattern with arity >=1, it is called as a copy with
%%%%      the first argument instantiated to the subset (represented
%%%%      as ordered list)
%%%%
%%%% Enumerate all minimal subsets of Set that satisfy Test,
%%%% 
%%%% Example call:
%%%% 
testpred([b,c]) :-
 	!.    
testpred([b,c,d]) :-
 	!.
testpred(X) :-
 	writeln(X),
 	fail.
%%%% 
%%%% ?- minimal_subset([a,b,c,d], testpred(_), X)
%%%% 
%%%% 
%%%% Probably this could be implemented more efficiently and elegantly.
%%%% 

minimal_subset(Set, Test, X) :-
	sort(Set, Set1),
	ms([[]-Set1|R], R, Test, [], X).
	
ms(Q, R, Test, SoFar, X) :-
	Q \== R,
	Q = [E-Y|Q1],
	copy_term(Test, Call),
	arg(1, Call, E),
	( Call ->
	  ( X = E  
	  ; ms(Q1, R, Test, [E|SoFar], X)
	  )
	; 
	  m_ms(Y, E, SoFar, R, R1),
	  ms(Q1, R1, Test, SoFar, X)
	).

m_ms(Y, X, SoFar, R, R1) :-
	findall([E|X]-U,
		(select(E, Y, U), (X = []; X=[F|_], E @< F),
		    \+ (member(SF, SoFar), ord_subset(SF, [E|X]))), Pairs),
	append(Pairs, R1, R).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% what exactly is done here?
%% 
simp_nf(M, M1) :-
	red_subsres(M, M0), %% ?
	map_i_srt(M0, M2),
	sort(M2, M1).
%	simp_mods(M2a, M1).

map_i_srt([X|Xs], [X1|Xs1]) :-
	i_srt(X, X1),
	map_i_srt(Xs, Xs1).
map_i_srt([], []).
	
simp_mods(Ms, Ms1) :-
	simp_mods_step(Ms, Ms2),
	!,
	simp_mods(Ms2, Ms1).
simp_mods(Ms, Ms).

simp_mods_step(Ms, [M2|Ms1]) :-
	select(M, Ms, Ms0),
	select(L, M, M2),
	( L = ~L1 ->
	  true
	; L1 = ~L
	),
	select(M1, Ms0, Ms1),
	select(L1, M1, M2),
	!.

i_srt(S, S1) :-
	map_lkv(S, S2),
	keysort(S2, S3),
	map_lkv(S1, S3).

map_lkv([X|Xs], [X1|Xs1]) :-
	lkv(X, X1),
	map_lkv(Xs, Xs1).
map_lkv([], []).
	
lkv(~A, A-neg) :-
	!.
lkv(A, A-pos).


dnf_simp(F, M) :-
	dnf_prop(F, M1),
	simp_nf(M1, M).

cnf_simp(F, M) :-
	cnf_prop(F, M1),
	simp_nf(M1, M).

dnf_prime(F, M) :-
	dnf_prop(F, M1),
	m_prime(M1, M2),
	sort(M2, M).

cnf_prime(F, M) :-
	cnf_prop(F, M1),
	m_prime(M1, M2),
	sort(M2, M).
