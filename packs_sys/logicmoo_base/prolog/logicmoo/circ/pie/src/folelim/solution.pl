%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Copyright (C) 2019 Christoph Wernhard
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

:- module(solution, [ground_sol/4,
		     rigorous_sol/6,
		     apply_sol/5,
		     simp_sol/3]).

:- use_module(folelim(utils_fol)).
:- use_module(folelim(logop_fol)).
:- use_module(folelim(simp_fol)).
:- use_module(swilib(err)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% General Notes:
%%%% 
%%%% A solution "lambda Vs . S" is represented as a pair Vs-S of Prolog
%%%% variables and a "formula template" S, that is, a formula which these
%%%% variables may occur.
%%%%
%%%% Various forms of formula simplification are supported, controlled through
%%%% simp_... options. These are passed to simp_form/4 from module simp_fol.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% ground_sol(+F, +P/N, +Options, -Sol)
%%%%
%%%%   Computes a Boolean solution with a naive implementation of the Method
%%%%   from Eberhard, Hetzl and Weller: "Boolean unification with predicates",
%%%%   JLC, vol 27, 2017, pp. 109-128.
%%%%
%%%%   F:   Input formula, a quantifier-free first-order formula.
%%%%        some occurrences of quantifiers are tolerated (an
%%%%        error is raised if the quantification can not be
%%%%        handled by the method).
%%%%
%%%%   P/N: The unknown predicate and its arity.
%%%%
%%%%   Options: List of key=value items, including the following keys:
%%%%  
%%%%      mode: value = *p* | n | s
%%%%            Determines which variant of Ackermann's lemma is
%%%%            applied: positive, negative, the one leading to smallest
%%%%            subformula
%%%%
%%%%      sort_heuristics: value = *h1* | none
%%%%            Heuristics for combining subformulas.
%%%%
%%%%      simp_comb: value = *d5* | SIMP
%%%%      simp_sol:  value = fast | SIMP
%%%%            Formula simplifications used at combining subformulas
%%%%            and for the final result.
%%%%
%%%%   Sol: The output solution.
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
ground_sol(F, P/N, Options, Vs-S) :-
	( memberchk(mode=Mode, Options) -> true ; Mode=p ),
 	dnf5(F, F1),
	map_dclause_sol(F1, P/N, Mode, Sols),
	( memberchk(sort_heuristics=SHeur, Options) -> true ; SHeur=h1 ),
	sort_ground_sols(Sols, SHeur, Sols1),
	gen_functions(N, As, Vs1, Map),
	( memberchk(simp_comb=SimpComb, Options) -> true ; SimpComb=d5 ),
	combine_sols(Sols1, As, true, SimpComb, S1),
	( memberchk(simp_sol=SimpSol, Options) -> true ; SimpSol=fast ),
	simp_form(S1, SimpSol, 'ground_sol', S2),
	logform_rename_free_functions(S2, Map, S),
	Vs = Vs1.

sort_ground_sols(Sols, none, Sols) :-
	!.
sort_ground_sols(Sols, SHeur, Sols1) :-
	map_add_gs_sortkey(Sols, SHeur, Sols2),
	sort(Sols2, Sols3),
	map_val(Sols3, Sols1).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).

map_add_gs_sortkey([X|Xs], SHeur, [X1|Xs1]) :-
	add_gs_sortkey(X, SHeur, X1),
	map_add_gs_sortkey(Xs, SHeur, Xs1).
map_add_gs_sortkey([], _, []).

add_gs_sortkey(sol(F,X-G), h1, k(N1,N2)-sol(F,X-G)) :-
	!,
	%% a heuristics that produces small overall solutions in some examples
 	term_size_1(F, N1),
	term_size_1(G, N3),
	N2 is 1 / (N3 + 1).

term_size_1(X, 1) :- var(X), !.
term_size_1(X, 1) :- atomic(X), !.
term_size_1(X, N) :-
	compound(X),
	X =.. [_|Xs],
	map_term_size_1(Xs, 1, N).

map_term_size_1([], N, N).
map_term_size_1([X|Xs], N, N1) :-
	term_size_1(X, N2),
	N3 is N2 + N,
	map_term_size_1(Xs, N3, N1).
				
simp_in_combining(F, Simp, F1)  :-
	simp_form(F, Simp, 'Combining', F1).

combine_sols([], _, _, _, true).
combine_sols([sol(F,Xs-G)], A, Prev, Simp, F1) :-
	!,
	copy_term(Xs-G, A-G1),
	simp_in_combining(((Prev, F) -> G1), Simp, F1).
combine_sols([sol(F,Xs-G)|Ss], A, Prev, Simp, (F1, S)) :-
	writeq(user_error, sol(F,Xs-G)), nl(user_error),
	copy_term(Xs-G, A-G1),
	simp_in_combining(((Prev, F) -> G1), Simp, F1),
	combine_sols(Ss, A, (~F, Prev), Simp, S).

map_dclause_sol([X|Xs], PN, Mode, [sol(FS,S)|Xs1]) :-
	dclause_sol(X, PN, Mode, FS, S),
	map_dclause_sol(Xs, PN, Mode, Xs1).
map_dclause_sol([], _, _, []).

dclause_sol(C, P/N, Mode, FS, S) :-
	dclause_sol(C, P/N, FSA, XA-SA, FSB, XB-SB),
	( Mode = p ->
	  FS = FSA, S = XA-SA
	; Mode = n ->
	  FS = FSB, S = XB-SB
	; Mode = s ->
	  logform_size(FSA, S1),
	  logform_size(FSB, S2),
	  logform_size(SA, S3),
	  logform_size(SB, S4),
	  ( S1-S3 @=< S2-S4 ->
	    FS = FSA, S = XA-SA
	  ; FS = FSB, S = XB-SB
	  )
	).

c_negate([~L|Ls], [L|Ls1]) :-
	!,
	c_negate(Ls, Ls1).
c_negate([L|Ls], [~L|Ls1]) :-
	c_negate(Ls, Ls1).
c_negate([], []).

%%%% 
%%%% Computes the two Ackermann solutions for a disjunctive clause: FSA is the
%%%% dclause after eliminating with the first solution (returned as a
%%%% formula), SA is the first solution as a pair PrologVars-Formula, FSB, SB
%%%% analogous for the second solution
%%%%
dclause_sol(C, P/N, FSA, SA, FSB, SB) :-
	split_clause(C, P/N, Pos, Neg, None),
%%%% 	length(Pos, L1),
%%%% 	length(Neg, L2),
%%%% 	writeln(len_pn(L1,L2,Pos,Neg)),
	( (\+ ground(Pos) ; \+ ground(Neg)) ->
	  err('dclause_sol on nonground instance of ~q in ~q', [P/N, C])
	; true
	),
 	functor(PXs, P, N),
 	PXs =.. [_|Xs],
	( ground(None) ->
	  list_to_conjunction(None, None1)
	; c_negate(None, None2),
	  matrix_to_form_keep_skolems([None2], None3),
	  logform_negate(None3, None1)
	),
	%%
	val_form(Pos, Xs, ValP),
	subst_val_form(Neg, Xs, ValP, FNeg),
	logform_conjoin(FNeg, None1, FSA1),
	logform_negate(ValP, ValP1),
	simp_fol_fast(FSA1, FSA),
	SA = Xs-ValP1,
	%%
	val_form(Neg, Xs, ValN),
	subst_val_form(Pos, Xs, ValN, FPos),
	logform_conjoin(FPos, None1, FSB1),
	simp_fol_fast(FSB1, FSB),
	SB = Xs-ValN.

subst_val_form([], _, _, true).
subst_val_form([PA], Xs, Val, F) :-
	!,
	subst_val_form_1(PA, Xs, Val, F).
subst_val_form([PA|PAs], Xs, Val, (F,F1)) :-
	subst_val_form_1(PA, Xs, Val, F),
	subst_val_form(PAs, Xs, Val, F1).

subst_val_form_1(PA, Xs, Val, F) :-
	PA =.. [_|A],
	copy_term(Xs-Val, A-F).

val_form([], _, true).
val_form([PA], Xs, F) :-
	!,
	val_form_1(PA, Xs, F).
val_form([PA|PAs], Xs, (F,F1)) :-
	val_form_1(PA, Xs, F),
	val_form(PAs, Xs, F1).

val_form_1(PA, Xs, F) :-
	PA =.. [_|As],
	val_form_2(As, Xs, F).

val_form_2([], [], false).
val_form_2([A], [X], ~(X=A)) :-
	!.
val_form_2([A|As], [X|Xs], (~(X=A);F)) :-
	val_form_2(As, Xs, F).

split_clause([L|C], P/N, Pos, Neg, None) :-
	( L = ~A ->
	  ( functor(A, P, N) ->
	    Neg = [A|Neg1],
	    Pos = Pos1, None = None1
	  ; None = [L|None1],
	    Pos = Pos1, Neg = Neg1
	  )
	; functor(L, P, N) ->
	  Pos = [L|Pos1],
	  Neg = Neg1, None = None1
	; None = [L|None1],
	  Pos = Pos1, Neg = Neg1
	),
	split_clause(C, P/N, Pos1, Neg1, None1).
split_clause([], _, [], [], []).

%%%% 
%%%% simp_sol(+Vs-S, Options, -Ws-T)
%%%%
%%%%    Simplify a Boolean solution according to the value of
%%%%    key simp_sol in Options.
%%%%
simp_sol(X-S, Options, X1-S1) :-
	length(X, N),
	gen_functions(N, As, X1, Map),
	copy_term(X-S, As-S2),
	( memberchk(simp_sol=Simp, Options) -> true ; Simp=fast ),
	simp_form(S2, Simp, sol, S3),
	logform_rename_free_functions(S3, Map, S1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% 
%%%% rigorous_sol(+F, +P/N, +T, +InputSol, +Options, -Sol)
%%%%
%%%%    Construct the rigouros solution from a given solution.
%%%%    F is the problem formula, P/N specifies the original unknown
%%%%    T is the predicate name of the parameter to use,
%%%%    InputSol is the given particular solution.
%%%%
%%%%    Option check=true verifies that there are no variable collisions.
%%%%
rigorous_sol(F, P/N, T, X-G, Options, Y-R) :-
	TX =.. [T|X],
	apply_sol(F, P/N, X-TX, Options, FT),
	R1 = ((G, ~FT) ; (TX, FT)),
	simp_sol(X-R1, Options, Y1-R),
	Y = Y1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%% 
%%%% apply_sol(+F, +P/N, +Sol, +Options, F1)
%%%% 
%%%%    Apply the given solution Sol to the given formula F for
%%%%    unknown P/N. That is, substitute all occurrences of the
%%%%    unknown predicate with Sol.
%%%%
%%%%    Also accepts as input solution Sol: X-S where X is variable,
%%%%    understood as [X]-S and just a formula S, understood as []-S.
%%%%
apply_sol(F, P/N, G, Options, F1) :-
	G \= (_-_),
	!,
	apply_sol(F, P/N, []-G, Options, F1).
apply_sol(F, P/N, X-G, Options, F1) :-
	( var(X) -> X0=[X] ; X0=X ),
	( memberchk(check=true, Options) ->
	  apply_sol_chk(F, P/N, X0-G, Options, F1)
	; logform_process_subforms(F, subst_pred(P/N,X0-G), F2),
	  ( memberchk(simp_apply=Simp, Options) -> true ; Simp=fast ),
	  simp_form(F2, Simp, apply_sol, F1)
	).

apply_sol_chk(F, P/N, X-G, Options, F1) :-
	copy_term(X-G, X1-G1),
	gen_functions(X1),
	f_signature(G1, _, Funs),
	findall(Fun, member(Fun/_, Funs), Funs1),
	subtract(Funs1, X1, Funs2),
	logform_process_subforms_with_bindings(F,
					       subst_pred_chk(P/N,X-G,Funs2),
					       F2),
	( memberchk(simp_apply=Simp, Options) -> true ; Simp=fast ),
	simp_form(F2, Simp, apply_sol_chk, F1).

subst_pred(P/N, X-G, A, B) :-
	logform_is_atom(A),
	functor(A, P, N),
	!,
	A =.. [_|Args],
	copy_term(X-G, Args-B).
subst_pred(_, _, A, A).

subst_pred_chk(P/N, X-G, Funs, A, Bindings, B) :-
	logform_is_atom(A),
	functor(A, P, N),
	!,
	( member(Fun, Funs), logform_bindings_memberchk(Fun, Bindings) ->
	  err('Attempt to substitute with bound symbol ~q in ~q', [Fun, A])
	; true
	),
	A =.. [_|Args],
	copy_term(X-G, Args-B).
subst_pred_chk(_, _, _, A, _, A).

gen_functions(0, [], [], []) :-
	!.
gen_functions(N, [S|Ss], [V|Vs], [S-V|Map]) :-
	N > 0,
	logform_gen_function(S),
	N1 is N-1,
	gen_functions(N1, Ss, Vs, Map).

gen_functions(0, []) :-
	!.
gen_functions(N, [S|Ss]) :-
	N > 0,
	logform_gen_function(S),
	N1 is N-1,
	gen_functions(N1, Ss).

gen_functions([]).
gen_functions([X|Xs]) :-
	logform_gen_function(X),
	gen_functions(Xs).

