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


:- module(prettysymbols_fol, [logform_vars_to_pretty_symbols/2,
			      n_pretty_vars/2,
			      n_pretty_vars/3,
			      prettify_imp/2]).
			      
:- use_module(swilib(err)).
:- use_module(folelim(logop_fol)).
:- use_module(folelim(tform)).
:- use_module(folelim(auxiliary_fol)).
:- use_module(folelim(unskolemize)).
:- use_module(folelim(preprocexp), [m_red_condense/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_pretty_vars(N, Vs) :-
	     n_pretty_vars(N, 0, Vs).

n_pretty_vars(N, From, Vs) :-
	N1 is From + N,
	npv_1(From, N1, Vs).

npv_1(N, M, []) :-
	N >= M,
	!.
npv_1(N, M, [X|Xs]) :-
	nth_var_name(N, X),
	N1 is N+1,
	npv_1(N1, M, Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Renames bound symbols, i.e. individual and predicate variables.  Re-uses
%%%% the variable symbols if they are not bound in the context. Thus the
%%%% result is not "clean", i.e. the same variable might be bound by different
%%%% quantifiers. However this is usually good to read.  Symbols free in the
%%%% input are never used as variable symbols.
%%%% 
logform_vars_to_pretty_symbols(F, F1) :-
	( term_variables(F, Vs), Vs \= [] ->
	  err('Applied logform_vars_to_pretty_symbols to formula with Prolog variables')
	; true
	),
	so_to_t(F, F2),
	logform_clean_vars_to_prolog_vars(F2, F3),
	collision_functors(F3, Exclude),
	logform_destruct_subforms_with_bindings(F3, destructive_vars_to_pretty(Exclude)),
	t_to_so(F3, F4),
	prettify_singleton_quantifiers(F4, F1).

destructive_vars_to_pretty(E, F, B) :-
	dvtp(F, E, B).
	
dvtp(ex2(X, _), E, B) :-
	!,
	logupon_list(X, X2),
	instantiate_pred_vars(X2, E-B, 0, _).
dvtp(all2(X, _), E, B) :-
	!,
	logupon_list(X, X2),
	instantiate_pred_vars(X2, E-B, 0, _).
dvtp(ex(X, _), E, B) :-
	!,
	logupon_list(X, X2),
	instantiate_vars(X2, E-B, 0, _).
dvtp(all(X, _), E, B) :-
	!,
	logupon_list(X, X2),
	instantiate_vars(X2, E-B, 0, _).
dvtp(_, _, _).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% A general formula iterator, operates outside-in, called just for side
%%%% effects (hence the name "destruct") - i.e. binding of variables.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_destruct_subforms_with_bindings(F, Pred) :-
	lrs_oib(F, [], Pred).

mk_call(Pred, X1, X2, Call) :-
	atom(Pred),
	!,
	Call =.. [Pred,X1,X2].
mk_call(Pred, X1, X2, Call) :-
	Pred =.. Pred1,
	append(Pred1, [X1, X2], Call1),
	Call =.. Call1.

lrs_oib(F, B, Pred) :-
	functor(F, Op, 1),
	logop_unary(Op),
	!,
	mk_call(Pred, F, B, Call),
	call(Call),
	arg(1, F, F2),
	lrs_oib(F2, B, Pred).
lrs_oib(F, B, Pred) :-
	functor(F, Op, 2),
	logop_binary(Op),
	!,
	mk_call(Pred, F, B, Call),
	call(Call),
	arg(1, F, F2),
	lrs_oib(F2, B, Pred),
	arg(2, F, F4),
	lrs_oib(F4, B, Pred).
lrs_oib(F, B, Pred) :-
	functor(F, Op, 2),
	logop_quantifier(Op),
	!,
	mk_call(Pred, F, B, Call),
	call(Call),
	( logform_binder(F, B2) ->
	  append(B2, B, B3)
	; B3 = B
	),
	arg(2, F, F2),
	lrs_oib(F2, B3, Pred).
lrs_oib(F, B, Pred) :-
	mk_call(Pred, F, B, Call),
	call(Call).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

collision_functors(F, Ks) :-
	findall(K, ( sub_term(K1, F),
		     \+ var(K1),
		     functor(K1, K, _),
		     is_collision_functor(K1)
		   ),
		Ks1),
	sort(Ks1, Ks).

is_collision_functor(X) :- atom(X), atom_prefix(X, x), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, y), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, z), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, u), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, v), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, w), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, p), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, q), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, r), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, s), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, f), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, g), !.
is_collision_functor(X) :- atom(X), atom_prefix(X, h), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

instantiate_vars([X|Xs], F, N, N1) :-
	nth_var_name(N, X1),
	( sub_term(T, F), \+ var(T), functor(T, X1, _) ->
	  N2 is N+1,
	  instantiate_vars([X|Xs], F, N2, N1)
	; X = X1,
	  N2 is N+1,
	  instantiate_vars(Xs, F, N2, N1)
	).
instantiate_vars([], _, N, N).
	
nth_var_name(N, X) :-
	K is N mod 6,
	( K = 0 ->
	  A = x
	; K = 1 ->
	  A = y
	; K = 2 ->
	  A = z
	; K = 3 ->
	  A = u
	; K = 4 ->
	  A = v
	; K = 5 ->
	  A = w
	),
	C is N div 6,
	( C = 0 ->
	  X = A
	; concat_atom([A,C], X)
	).

instantiate_pred_vars([X|Xs], F, N, N1) :-
	nth_pred_var_name(N, X1),
	( sub_term(T, F), \+ var(T), functor(T, X1, _) ->
	  N2 is N+1,
	  instantiate_pred_vars([X|Xs], F, N2, N1)
	; X = X1,
	  N2 is N+1,
	  instantiate_pred_vars(Xs, F, N2, N1)
	).
instantiate_pred_vars([], _, N, N).

nth_pred_var_name(N, X) :-
	K is N mod 3,
	( K = 0 ->
	  A = p
	; K = 1 ->
	  A = q
	; K = 2 ->
	  A = r
	),
	C is N div 3,
	( C = 0 ->
	  X = A
	; concat_atom([A,C], X)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prettystruct(F, F1) :-
	functor(F, Op, _),
	logop_quantifier(Op),
	!,
	F =.. [Op,Upon,Arg],
	prettystruct(Arg, Arg1),
	F1 =.. [Op,Upon,Arg1].
prettystruct((F,G), FG1) :-
	!,
	conj_list((F,G), FG2),
	map_prettystruct(FG2, FG3),
	sort_prettystruct(FG3, NEG_FG4, POS_FG4, Contra),
	( ( Contra=true ; memberchk(false, FG3) ; memberchk(~true, FG3) ) ->
	  FG1 = false
	; append(POS_FG4, NEG_FG4, FG5),
	  list_conj(FG5, FG1)
	).
prettystruct((F;G), FG1) :-
	!,
	disj_list((F;G), FG2),
	map_prettystruct(FG2, FG3),
	sort_prettystruct(FG3, NEG_FG4, POS_FG4, Contra),
	( ( Contra = true; memberchk(true, FG3) ; memberchk(~false, FG3) ) ->
  	  FG2 = true
	; list_disj(POS_FG4, POS_FG5),
	  ( NEG_FG4 = [] ->
	    FG1 = POS_FG5
	  ; map_negate(NEG_FG4, NEG_FG4_N),
	    list_conj(NEG_FG4_N, NEG_FG5),
	    FG1 = (NEG_FG5 -> POS_FG5)
	  )
	).
prettystruct(F, F).
	
map_prettystruct([X|Xs], [X1|Xs1]) :-
	prettystruct(X, X1),
	map_prettystruct(Xs, Xs1).
map_prettystruct([], []).

map_negate([X|Xs], [X1|Xs1]) :-
	negate(X, X1),
	map_negate(Xs, Xs1).
map_negate([], []).

sort_prettystruct(Fs, NegFs, PosFs, Contra) :-
	map_add_spf_key(Fs, Fs1),
	sort(Fs1, Fs2),
	map_val(Fs2, Fs3),
	split_neg_pos(Fs3, NegFs, PosFs),
	( member(~X, NegFs),
	  member(Y, PosFs),
	  X == Y ->
	  Contra = true
	; Contra = false
	).

split_neg_pos([~F|Fs], [~F|Fs1], Gs1) :- !, split_neg_pos(Fs, Fs1, Gs1).
split_neg_pos([F|Fs], Fs1, [F|Gs1]) :- !, split_neg_pos(Fs, Fs1, Gs1).
split_neg_pos([], [], []).

map_add_spf_key([X|Xs], [X1|Xs1]) :-
	add_spf_key(X, X1),
	map_add_spf_key(Xs, Xs1).
map_add_spf_key([], []).

add_spf_key(F, k(Sign,Size)-F) :-
	logform_size(F, Size),
	( F = ~_ -> Sign = n
	; Sign = p
	).
	
% map_neg([X|Xs], [X1|Xs1]) :-
% 	neg(X, X1),
% 	map_neg(Xs, Xs1).
% map_neg([], []).

negate(true, false) :- !.
negate(false, true) :- !.
negate(~F, F) :- !.
negate(F, ~F).
       
disj_list(F, L) :-
	disj_list(F, [], L).
disj_list((F;G), L, L1) :-
	!,
	disj_list(F, L, L2),
	disj_list(G, L2, L1).
disj_list(F, L, [F|L]).

conj_list(F, L) :-
	conj_list(F, [], L).
conj_list((F,G), L, L1) :-
	!,
	conj_list(F, L, L2),
	conj_list(G, L2, L1).
conj_list(F, L, [F|L]).

list_disj([], false) :- !.
list_disj([F], F) :- !.
list_disj([F|Fs], (F;Fs1)) :- list_disj(Fs, Fs1).

list_conj([], false) :- !.
list_conj([F], F) :- !.
list_conj([F|Fs], (F,Fs1)) :- list_conj(Fs, Fs1).

map_val([_-X|Xs], [X|Xs1]) :-
	map_val(Xs, Xs1).
map_val([], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inline_equality

prettify_imp(F, F1) :-
	logform_process_subforms(F, introduce_implications, F1).

%%%% 
%%%% Experimental:
%%%%
introduce_implications((~F;(G->H)), ((F,G)->H)) :- !.
introduce_implications((F;(G->H)), (G->(F;H))) :- !.
introduce_implications((~F;~G), ~((F,G))):- !.
introduce_implications((~F;G), (F->G)) :- !.
introduce_implications((F;~G), (G->F)) :- !.
% introduce_implications(all(X, ~F), ~ex(X, F)) :- !.
introduce_implications(ex(X, (F,~G)), ~(all(X, (F -> G)))) :- !.
introduce_implications(ex(X, (~F,G)), ~(all(X, (G -> F)))) :- !.
introduce_implications(X, X).

% matrix_to_imp_form([], true) :-
% 	!.
% matrix_to_imp_form([C], C1) :-
% 	!,
% 	clause_to_imp_form(C,C1).
% matrix_to_imp_form([C|Cs], (C1 , Cs1)) :-
% 	clause_to_imp_form(C,C1),
% 	matrix_to_imp_form(Cs, Cs1).
% 
% clause_to_imp_form(C, F) :-
% 	copy_term(C, C2),
% 	term_variables(C2, Vs),
% 	cm_vars(Vs, 1),
% 	split_clause_pn(C2, CP, CN),
% 	( CP = [] ->
% 	  ( CN = [] ->
% 	    F2 = false
% 	  ; list_to_andseq_1(CN, F1),
% 	    F2 = ~F1
% 	  )
% 	; CN = [] ->
% 	  list_to_orseq_1(CP, F2)
% 	; list_to_orseq_1(CP, F3),
% 	  list_to_andseq_1(CN, F4),
% 	  F2 = (F4 -> F3)
% 	),
% 	( Vs = [] ->
% 	  F = F2
%         ; F = all(Vs, F2)
%         ).
% 
% list_to_andseq_1([],true).
% list_to_andseq_1([X],X) :-
% 	!.
% list_to_andseq_1([X,Y|Z],(X,YZ1)) :- 
% 	list_to_andseq_1([Y|Z],YZ1).
% 
% list_to_orseq_1([],false).
% list_to_orseq_1([X],X) :-
% 	!.
% list_to_orseq_1([X,Y|Z],(X;YZ1)) :- 
% 	list_to_orseq_1([Y|Z],YZ1).
% 
% cm_vars([], _).
% cm_vars([X|Xs], N) :-
% 	concat_atom(['$x',N],X),
% 	N1 is N+1,
% 	cm_vars(Xs, N1).
% 
% split_clause_pn([~A|C], CP, [A|CN]) :-
% 	!,
% 	split_clause_pn(C, CP, CN).
% split_clause_pn([A|C], [A|CP], CN) :-
% 	split_clause_pn(C, CP, CN).
% split_clause_pn([], [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prettify_singleton_quantifiers(F, F1) :-
	logform_process_subforms(F, psq, F1).

psq(ex2([X], F), ex2(X, F)) :- !.
psq(all2([X], F), all2(X, F)) :- !.
psq(ex([X], F), ex(X, F)) :- !.
psq(all([X], F), all(X, F)) :- !.
psq(F, F).
