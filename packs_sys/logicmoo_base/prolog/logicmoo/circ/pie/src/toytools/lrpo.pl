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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% LRPO Version: 1.0 Patchlevel: %I% Date: %G%
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% History:
%%%% 
%%%% Tue Aug 10 17:48:58 2010
%%%% copy of provers/rewrite/lrpo.pl
%%%% 

:- module(lrpo, 
          [ install_lrpo_ordering/1,
	    default_lrpo_ordering/2,
	    default_lrpo_ordering/3,
	    lrpo_greater/2
	  ]).

:- use_module(library(occurs)).
:- use_module(swilib(err)).

:- dynamic(lrpo_positions/3).
:- dynamic(lrpo_index/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% LRPO
%%%% 
%%%% Oriented at lrpo.c in the OTTER 3.04 source (and the prog synthesis 
%%%% paper by Dershowitz and Reddy in J.Symb.Comp.1993,15,p476-494 which
%%%% i had just at hand).
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% install_lrpo_ordering(Precedence_List).
%%%% 
%%%%     PrecedenceList contains elements of the form:
%%%%        Functor/Arity-Positions | 
%%%%        Functor/Arity 
%%%% 
%%%%    Positions is a list of numbers specifying the order of argument
%%%%    positions used in lexical comparision. Default: left to right.
%%%%    [Might also be a subset of actual argument positions, any lrpo
%%%%    stuff is then performed only on these arguments]
%%%%
%%%%    Ordering of PrecedenceList specifies the lrpo symbol ordering
%%%%    (smallest_first).
%%%%
%%%%    All symbols occuring the term comparision should be declared with
%%%%    this predicate.
%%%%
%%%% default_lrpo_ordering(Functions, Precedence_List).
%%%% default_lrpo_ordering(Key, Functions, Precedence_List).
%%%%
%%%%    Functions is a list of Functor/Arity items. Precedence list is a
%%%%    precedence list suitable as input to install lrpo ordering. The
%%%%    default ordering is currently determined as in Otter 3.0.
%%%%
%%%%    Key is one of default, otter, exp1
%%%%
%%%% lrpo_greater(T1, T2).
%%%% 
%%%%    Succeeds if T1 is lrpo-greater than T2. Fails if it is lower,
%%%%    equal or ordering cannot be determined due to lack of
%%%%    instantiation.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

install_lrpo_ordering(Precedence_List) :-
	( PrecedenceList \= [_|_],  PrecedenceList \= [] ->
	  err('ERROR: Bad lrpo ordering specifier: ~q.',
	      [PrecedenceList]), 
	  fail
        ; true
        ),
	map_ilr(Precedence_List, 1, LrpoIndex),
	map_ilp(Precedence_List, LrpoPositions),
	compile_term(LrpoIndex, lrpo),
	compile_term(LrpoPositions, lrpo).

lrpo_default_positions(F, N, Pos) :-
	dp(F, N, [], Pos).

dp(_, 0, Pos, Pos) :-
	!.
dp(F, N, Pos, Pos1) :-
	N1 is N - 1,
	dp(F, N1, [N|Pos], Pos1).

map_ilp([], []).
map_ilp([Spec | Specs], [lrpo_positions(F, N, Pos) | Poss]) :-
	( Spec = F/N ->
	  lrpo_default_positions(F, N, Pos)
        ; Spec = F/N-Pos ->
	  check_positions(Pos, F, N)
        ),
	map_ilp(Specs, Poss).

check_positions([], _, _) :- !.
check_positions([P|Ps], F, N) :-
	!,
	( number(P),
          P >= 1,
	  P =< N ->
	  check_positions(Ps, F, N) 
        ; nl, write('ERROR: Bad lrpo position specification: '),
	  writeq(P), write(' for: '), writeq(F/N), writeln('.'),
	  fail
        ).
check_positions(X, F, N) :-
	nl, write('ERROR: Bad lrpo positions specification: '),
	writeq(X), write(' for: '), writeq(F/N), writeln('.'),
	fail.

map_ilr([], _, []).
map_ilr([Spec | Specs], I, [lrpo_index(F, N, I) | LIs]) :-
	( Spec = F/N -> true
        ; Spec = F/N-_ -> true
        ; nl, write('ERROR: Bad lrpo specification item: '), 
	  writeq(Spec), writeln('.'),
	  fail
        ),
	I1 is I + 1,
	map_ilr(Specs, I1, LIs).

lrpo_sym_greater(Sym1, N1, Sym2, N2) :-
	lrpo_index(Sym1, N1, I1),
	lrpo_index(Sym2, N2, I2),
	I1 > I2.

lrpo_greater(T1, _T2) :-
	var(T1),
	!,
	fail.
lrpo_greater(T1, T2) :-
	var(T2),
	!,
	contains_var(T2, T1).
lrpo_greater(T1, T2) :-
	functor(T1, F1, N1),
	functor(T2, F2, N2),
	( F1 = F2, N1 = N2 ->
	  lrpo_positions(F1, N1, Positions1),
	  lrpo_lex(T1, T2, Positions1)
        ; lrpo_sym_greater(F1, N1, F2, N2) ->
	  lrpo_positions(F2, N2, Positions2),
	  lrpo_greater_than_each_arg(T1, T2, Positions2)
        ; lrpo_positions(F1, N1, Positions1),
          lrpo_has_greaterq_arg(T1, T2, Positions1)
        ).
	     
lrpo_lex(_T1, _T2, []) :-
	!,
	fail.
lrpo_lex(T1, T2, [N|Ns]) :-
	arg(N, T1, T1N),
	arg(N, T2, T2N),
	( T1N == T2N ->
	  lrpo_lex(T1, T2, Ns)
        ; lrpo_greater(T1N, T2N) ->
	  lrpo_greater_than_each_arg(T1, T2, Ns)
        ; lrpo_has_greaterq_arg(T1, T2, Ns)
        ). 

lrpo_greater_than_each_arg(_, _, []) :-
	!.
lrpo_greater_than_each_arg(T1, T2, [N|Ns]) :-
	arg(N, T2, T2N),
	lrpo_greater(T1, T2N),
	lrpo_greater_than_each_arg(T1, T2, Ns).

lrpo_has_greaterq_arg(_T1, _T2, []) :-
	!,
	fail.
lrpo_has_greaterq_arg(T1, T2, [N|_]) :-
	arg(N, T1, T1N),
	( T1N == T2 ->
	  true
        ; lrpo_greater(T1N, T2)
        ),
	!.
lrpo_has_greaterq_arg(T1, T2, [_|Ns]) :-
	lrpo_has_greaterq_arg(T1, T2, Ns).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

default_lrpo_ordering(Functions, PrecedenceList) :-
	default_lrpo_ordering(default, Functions, PrecedenceList).

default_lrpo_ordering(default, Functions, PrecedenceList) :-
	!,
	default_lrpo_ordering_otterlike(Functions, PrecedenceList).
default_lrpo_ordering(otter, Functions, PrecedenceList) :-
	!,
	default_lrpo_ordering_otterlike(Functions, PrecedenceList).
default_lrpo_ordering(exp1, Functions, PrecedenceList) :-
	!,
	default_lrpo_ordering_exp1(Functions, PrecedenceList).
default_lrpo_ordering(Key, _, _) :-
	err('Bad lrpo ordering specifier: ~q.', [Key]).

default_lrpo_ordering_otterlike(Functions, PrecedenceList) :-
	%% after Otter 3.0
	maplist(lrpo_default_val, Functions, Fs1),
	sort(Fs1, Fs2),
	remove_keys(Fs2, PrecedenceList).

lrpo_default_val(F/0, 0.0-F/0) :- !.
lrpo_default_val(F/N, N1-F/N) :-
	N1 is 1.0 / N.

default_lrpo_ordering_exp1(Functions, PrecedenceList) :-
	%% After Prover 9, but not considering number of occurrences
	maplist(lrpo_default_val_exp1, Functions, Fs1),
	sort(Fs1, Fs2),
	remove_keys(Fs2, PrecedenceList).

lrpo_default_val_exp1(F/N, k(A,S,SI)-F/N) :-
	( N = 0 -> A = 0
	; N = 1 -> A = 2
	; N = 2 -> A = 1
	; A = N
	),
	( skolem_symbol(F, SI) ->
	  S = 1
	; S = 0, SI = 0
	).
	
skolem_symbol(F, Index) :-
	sub_atom(F, 0, _, _, sk),
	!,
	sub_atom(F, 2, _, 0, N),
	atom_number(N, Index).

remove_keys([_-V|Vs], [V|Vs1]) :-
	remove_keys(Vs, Vs1).
remove_keys([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compile_term(Clauses, Module) :-
	findall(P/N, ( member(Clause, Clauses),
	               ( Clause = (H :- _) ->
			 true
		       ; Clause = (:- _) ->
		         fail
		       ; Clause = H
		       ),
		       functor(H, P, N)
		     ),
		PNs),
	sort(PNs, PNs1),
	( member(P1/N1, PNs1),
	  functor(Template, P1, N1),
	  Module:retractall(Template),
	  fail
	; true
	),
	( member(Clause, Clauses),
	  ( Clause = (:- Declaration) ->
	    Module:call(Declaration)
	  ; Module:assert(Clause)
	  ),
	  fail
	; true
	).



