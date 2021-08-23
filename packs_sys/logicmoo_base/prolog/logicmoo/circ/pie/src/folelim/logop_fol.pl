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

:- module(logop_fol,
	  [ logop_constant/1,
	    logop_unary/1,
	    logop_binary/1,
	    logop_quantifier/1,

	    logupon_list/2,
	    logform_binder/2,
	    
	    logform_is_atom/1,
	    logform_is_compound/1,

	    logform_size/2,
	    
	    logform_has_first_order_quantifier/1,
	    logform_has_second_order_quantifier/1,
	    
	    logform_to_nnf/2,
	    logform_enum_atoms/2,
	    logform_enum_atoms/3,
	    logform_enum_atoms/4,
	    logform_enum_subformulas/2,
	    logform_enum_subformulas/3,
	    logform_enum_subformulas/4,
	    logform_enum_subformulas_with_place/6,
	    logform_process_subforms/3,
	    logform_process_subforms_with_polarity/3,
	    logform_process_subforms_with_bindings/3,
	    logform_process_subforms_with_polarity_and_bindings/3,
	    logform_bindings_memberchk/2,

	    logform_set_p_counter/1,
	    logform_set_x_counter/1,
	    logform_set_sk_counter/1,
	    logform_reset_counters/0,

	    logform_gen_predicate/1,
	    logform_gen_function/1,
	    logform_gen_symbol/1,
	    logform_gen_skolem_functor/1,
	    logform_is_skolem_functor/1,
	    
	    logform_rename_free_predicates/4,
	    logform_rename_free_functions/3,

	    logform_clean_vars/2,
	    logform_clean_vars_to_symbols/2,
	    logform_clean_vars_to_prolog_vars/2,
	    logform_gather_quantifiers/2,

	    logform_negate/2,
	    logform_conjoin/3,
	    logform_disjoin/3,
	    logform_allquant/3,
	    logform_exquant/3
	     
	    ] ).

:- module_transparent logform_process_subforms/3.
:- module_transparent lrs_io/3.

:- module_transparent logform_process_subforms_with_polarity/3.
:- module_transparent lrs_iop/4.

:- module_transparent logform_process_subforms_with_bindings/3.
:- module_transparent lrs_iob/4.

:- module_transparent logform_process_subforms_with_polarity_and_bindings/3.
:- module_transparent lrs_iopb/5.

:- use_module(swilib(sysdep)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logop_constant('true').
logop_constant('false').

logop_unary('~').

logop_binary(',').
logop_binary(';').
logop_binary('<->').
logop_binary('<-').
logop_binary('->').
logop_binary('&').
logop_binary('|').

pol_pattern('~', n).
pol_pattern('<-', p, n).
pol_pattern('->', n, p).
pol_pattern('<->', pn, pn).

pol_expansion((F <-> G), p, ((F->G),(G->F))).
pol_expansion((F <-> G), n, ((F,G);(~F,~G))).

logop_quantifier(all).
logop_quantifier(ex).
logop_quantifier(all2).
logop_quantifier(ex2).
logop_quantifier(alla).
logop_quantifier(exa).

logform_binder(F, Xs) :-
	functor(F, Q, 2),
	logop_quantifier(Q),
	!,
	arg(1, F, X),
	logupon_list(X, Xs),
	Xs \= [].

logupon_list(X, Xs) :-
	( var(X) ->
	  Xs = [X] 
	; X = [] ->
	  Xs = []
	; X = [_|_] ->
	  Xs = X
	; Xs = [X]
	).

logform_is_atom(F) :-
	\+ logform_is_compound(F).

logform_is_compound(F) :-
	functor(F, P, N),
	( N=0 -> logop_constant(P)
	; N=1 -> logop_unary(P)
	; N=2 -> ( logop_binary(P) -> true
		 ; logop_quantifier(P)
		 )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% The "process_subforms" predicates operate by rewriting inside-out
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_process_subforms(F, Pred, F1) :-
	lrs_io(F, Pred, F1).

logform_process_subforms_with_polarity(F, Pred, F1) :-
	lrs_iop(F, p, Pred, F1).

logform_process_subforms_with_bindings(F, Pred, F1) :-
	lrs_iob(F, [], Pred, F1).

logform_process_subforms_with_polarity_and_bindings(F, Pred, F1) :-
	lrs_iopb(F, p, [], Pred, F1).

lrs_io(F, Pred, F1) :-
	functor(F, Op, 1),
	logop_unary(Op),
	!,
	arg(1, F, F2),
	lrs_io(F2, Pred, F3),
	F4 =.. [Op,F3],
	mk_call(Pred, F4, F1, Call),
	call(Call).
lrs_io(F, Pred, F1) :-
	functor(F, Op, 2),
	logop_binary(Op),
	!,
	arg(1, F, F2),
	lrs_io(F2, Pred, F3),
	arg(2, F, F4),
	lrs_io(F4, Pred, F5),		
	F6 =.. [Op,F3,F5],
	mk_call(Pred, F6, F1, Call),
	call(Call).
lrs_io(F, Pred, F1) :-
	functor(F, Op, 2),
	logop_quantifier(Op),
	!,
	arg(1, F, X),
	arg(2, F, F2),
	lrs_io(F2, Pred, F3),
	F4 =.. [Op,X,F3],
	mk_call(Pred, F4, F1, Call),
	call(Call).
lrs_io(F, Pred, F1) :-
	mk_call(Pred, F, F1, Call),
	call(Call).

lrs_iop(F, P, Pred, F1) :-
	pol_expansion(F, P, G),
	!,
	lrs_iop(G, P, Pred, F2),
	( F2 == G ->
	  F1 = F
	; F1 = F2
	).
lrs_iop(F, P, Pred, F1) :-
	functor(F, Op, 1),
	logop_unary(Op),
	!,
	( pol_pattern(Op, PA) -> true ; PA = p ),
	set_pol(PA, P, P2),
	arg(1, F, F2),
	lrs_iop(F2, P2, Pred, F3),
	F4 =.. [Op,F3],
	mk_call(Pred, F4, P, F1, Call),
	call(Call).
lrs_iop(F, P, Pred, F1) :-
	functor(F, Op, 2),
	logop_binary(Op),
	!,
	( pol_pattern(Op, PA1, PA2) -> true
	; PA1 = p, PA2 = p
	),
	set_pol(PA1, P, P3),
	set_pol(PA2, P, P4),
	arg(1, F, F2),
	lrs_iop(F2, P3, Pred, F3),
	arg(2, F, F4),
	lrs_iop(F4, P4, Pred, F5),	
	F6 =.. [Op,F3,F5],
	mk_call(Pred, F6, P, F1, Call),
	call(Call).
lrs_iop(F, P, Pred, F1) :-
	functor(F, Op, 2),
	logop_quantifier(Op),
	!,
	arg(1, F, X),
	arg(2, F, F2),
	lrs_iop(F2, P, Pred, F3),
	F4 =.. [Op,X,F3],
	mk_call(Pred, F4, P, F1, Call),
	call(Call).
lrs_iop(F, P, Pred, F1) :-
	mk_call(Pred, F, P, F1, Call),
	call(Call).


lrs_iob(F, B, Pred, F1) :-
	functor(F, Op, 1),
	logop_unary(Op),
	!,
	arg(1, F, F2),
	lrs_iob(F2, B, Pred, F3),
	F4 =.. [Op,F3],
	mk_call(Pred, F4, B, F1, Call),
	call(Call).
lrs_iob(F, B, Pred, F1) :-
	functor(F, Op, 2),
	logop_binary(Op),
	!,
	arg(1, F, F2),
	lrs_iob(F2, B, Pred, F3),
	arg(2, F, F4),
	lrs_iob(F4, B, Pred, F5),		
	F6 =.. [Op,F3,F5],
	mk_call(Pred, F6, B, F1, Call),
	call(Call).
lrs_iob(F, B, Pred, F1) :-
	functor(F, Op, 2),
	logop_quantifier(Op),
	!,
	( logform_binder(F, B2) ->
	  append(B2, B, B3)
	; B3 = B
	),
	arg(1, F, X),
	arg(2, F, F2),
	lrs_iob(F2, B3, Pred, F3),
	F4 =.. [Op,X,F3],
	mk_call(Pred, F4, B, F1, Call),
	call(Call).
lrs_iob(F, B, Pred, F1) :-
	mk_call(Pred, F, B, F1, Call),
	call(Call).


lrs_iopb(F, P, B, Pred, F1) :-
	pol_expansion(F, P, G),
	!,
	lrs_iopb(G, P, B, Pred, F2),
	( F2 == G ->
	  F1 = F
	; F1 = F2
	).
lrs_iopb(F, P, B, Pred, F1) :-
	functor(F, Op, 1),
	logop_unary(Op),
	!,
	( pol_pattern(Op, PA) -> true ; PA = p ),
	set_pol(PA, P, P2),
	arg(1, F, F2),
	lrs_iopb(F2, P2, B, Pred, F3),
	F4 =.. [Op,F3],
	mk_call(Pred, F4, P, B, F1, Call),
	call(Call).
lrs_iopb(F, P, B, Pred, F1) :-
	functor(F, Op, 2),
	logop_binary(Op),
	!,
	( pol_pattern(Op, PA1, PA2) -> true
	; PA1 = p, PA2 = p
	),
	set_pol(PA1, P, P3),
	set_pol(PA2, P, P4),
	arg(1, F, F2),
	lrs_iopb(F2, P3, B, Pred, F3),
	arg(2, F, F4),
	lrs_iopb(F4, P4, B, Pred, F5),	
	F6 =.. [Op,F3,F5],
	mk_call(Pred, F6, P, B, F1, Call),
	call(Call).
lrs_iopb(F, P, B, Pred, F1) :-
	functor(F, Op, 2),
	logop_quantifier(Op),
	!,
	( logform_binder(F, B2) ->
	  append(B2, B, B3)
	; B3 = B
	),
	arg(1, F, X),
	arg(2, F, F2),
	lrs_iopb(F2, P, B3, Pred, F3),
	F4 =.. [Op,X,F3],
	mk_call(Pred, F4, P, B, F1, Call),
	call(Call).
lrs_iopb(F, P, B, Pred, F1) :-
	mk_call(Pred, F, P, B, F1, Call),
	call(Call).


mk_call(Pred, X1, X2, Call) :-
	atom(Pred),
	!,
	Call =.. [Pred,X1,X2].
mk_call(Pred, X1, X2, Call) :-
	Pred =.. Pred1,
	append(Pred1, [X1, X2], Call1),
	Call =.. Call1.

mk_call(Pred, X1, X2, X3, Call) :-
	atom(Pred),
	!,
	Call =.. [Pred,X1,X2,X3].
mk_call(Pred, X1, X2, X3, Call) :-
	Pred =.. Pred1,
	append(Pred1, [X1, X2, X3], Call1),
	Call =.. Call1.

mk_call(Pred, X1, X2, X3, X4, Call) :-
	atom(Pred),
	!,
	Call =.. [Pred,X1,X2,X3,X4].
mk_call(Pred, X1, X2, X3, X4, Call) :-
	Pred =.. Pred1,
	append(Pred1, [X1, X2, X3, X4], Call1),
	Call =.. Call1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_enum_subformulas(Form, Subform) :-
	les(Form, p, [], Subform, _, _).

logform_enum_subformulas(Form, Subform, Polarity) :-
	les(Form, p, [], Subform, Polarity, _).

logform_enum_subformulas(Form, Subform, Polarity, BoundSymbols) :-
	les(Form, p, [], Subform, Polarity, BoundSymbols).

logform_enum_subformulas_with_place(Form, Subform, Polarity, BoundSymbols,
				    NewForm, Place) :-
	les_with_place(Form, p, [], Subform, Polarity, BoundSymbols,
		       NewForm, Place).

les(F, P, B, F, P, B).
les(F, P, B, F1, P1, B1) :-
	functor(F, Op, 1),
	logop_unary(Op),
	!,
	( pol_pattern(Op, PA) -> true ; PA = p ),
	set_pol(PA, P, P2),
	arg(1, F, F2),
	les(F2, P2, B, F1, P1, B1).
les(F, P, B, F1, P1, B1) :-
	functor(F, Op, 2),
	logop_binary(Op),
	!,
	( pol_pattern(Op, PA1, PA2) -> true
	; PA1 = p, PA2 = p
	),
	( set_pol(PA1, P, P2),
	  arg(1, F, F2),
	  les(F2, P2, B, F1, P1, B1)
	; set_pol(PA2, P, P2),
	  arg(2, F, F2),
	  les(F2, P2, B, F1, P1, B1)
	).
les(F, P, B, F1, P1, B1) :-
	functor(F, Op, 2),
	logop_quantifier(Op),
	!,
	( logform_binder(F, B2) ->
	  append(B2, B, B3)
	; B3 = B1
	),
	arg(2, F, F2),
	les(F2, P, B3, F1, P1, B1).


les_with_place(F, P, B, F, P, B, NewF, NewF).
les_with_place(F, P, B, F1, P1, B1, NewF, Place) :-
	functor(F, Op, 1),
	logop_unary(Op),
	!,
	( pol_pattern(Op, PA) -> true ; PA = p ),
	set_pol(PA, P, P2),
	arg(1, F, F2),
	NewF =.. [Op,NewF1],
	les_with_place(F2, P2, B, F1, P1, B1, NewF1, Place).
les_with_place(F, P, B, F1, P1, B1, NewF, Place) :-
	functor(F, Op, 2),
	logop_binary(Op),
	!,
	( pol_pattern(Op, PA1, PA2) -> true
	; PA1 = p, PA2 = p
	),
	( set_pol(PA1, P, P2),
	  arg(1, F, F2),
	  arg(2, F, NewFOther),
	  NewF =.. [Op,NewF1,NewFOther],
	  les_with_place(F2, P2, B, F1, P1, B1, NewF1, Place)
	; set_pol(PA2, P, P2),
	  arg(2, F, F2),
	  arg(1, F, NewFOther),
	  NewF =.. [Op,NewFOther,NewF1],
	  les_with_place(F2, P2, B, F1, P1, B1, NewF1, Place)
	).
les_with_place(F, P, B, F1, P1, B1, NewF, Place) :-
	functor(F, Op, 2),
	logop_quantifier(Op),
	!,
	( logform_binder(F, B2) ->
	  append(B2, B, B3)
	; B3 = B1
	),
	arg(2, F, F2),
	arg(1, F, Upon),
	NewF =.. [Op,Upon,NewF1],
	les_with_place(F2, P, B3, F1, P1, B1, NewF1, Place).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_enum_atoms(Form, Atom, Polarity, BoundSymbols) :-
	logform_enum_atoms(Form, p, [], Atom, Polarity, BoundSymbols).
logform_enum_atoms(Form, Atom, Polarity) :-
	logform_enum_atoms(Form, p, [], Atom, Polarity, _).
logform_enum_atoms(Form, Atom) :-
	logform_enum_atoms(Form, p, [], Atom, _, _).

logform_enum_atoms(F, _, _, _, _, _) :-
	logop_constant(F),
	!,
	fail.
logform_enum_atoms(F, P, B, A, P1, B1) :-
	functor(F, Op, 1),
	logop_unary(Op),
	!,
	( pol_pattern(Op, PA) -> true ; PA = p ),
	set_pol(PA, P, P2),
	arg(1, F, F1),
	logform_enum_atoms(F1, P2, B, A, P1, B1).
logform_enum_atoms(F, P, B, A, P1, B1) :-
	functor(F, Op, 2),
	logop_binary(Op),
	!,
	( pol_pattern(Op, PA1, PA2) -> true
	; PA1 = p, PA2 = p
	),
	( set_pol(PA1, P, P2),
	  arg(1, F, F1),
	  logform_enum_atoms(F1, P2, B, A, P1, B1)
	; set_pol(PA2, P, P2),
	  arg(2, F, F1),
	  logform_enum_atoms(F1, P2, B, A, P1, B1)
	).
logform_enum_atoms(F, P, B, A, P1, B1) :-
	functor(F, Op, 2),
	logop_quantifier(Op),
	!,
	( logform_binder(F, B2) ->
	  append(B2, B, B3)
	; B3 = B1
	),
	arg(2, F, F1),
	logform_enum_atoms(F1, P, B3, A, P1, B1).
logform_enum_atoms(F, P, B, F, P, B).
	
set_pol(p, P, P).
set_pol(n, P, N) :- flip_pol(P, N).
set_pol(pn, _, pn).
	
flip_pol(p, n).
flip_pol(n, p).
flip_pol(pn, pn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_to_nnf(~true, false) :- !.
logform_to_nnf(~false, true) :- !.
logform_to_nnf(~(~F), F1) :- !,	logform_to_nnf(F, F1).
logform_to_nnf(~((F,G)), (NF;NG)) :-
	!,
	logform_to_nnf(~F, NF),
	logform_to_nnf(~G, NG).
logform_to_nnf(~((F;G)), (NF,NG)) :- !,
	logform_to_nnf(~F, NF),
	logform_to_nnf(~G, NG).
logform_to_nnf(~((F -> G)), FG1) :- !, logform_to_nnf((F,~G), FG1).
logform_to_nnf(~((F <- G)), FG1) :- !, logform_to_nnf((~F,G), FG1).
logform_to_nnf(~((F <-> G)), FG1) :- !, logform_to_nnf(((F;G),(~F;~G)), FG1).
logform_to_nnf(~ex(X, F), all(X, F1)) :- !, logform_to_nnf(~F, F1).
logform_to_nnf(~ex2(X, F), all2(X, F1)) :- !, logform_to_nnf(~F, F1).
logform_to_nnf(~exa(X, F), alla(X, F1)) :- !, logform_to_nnf(~F, F1).
logform_to_nnf(~all(X, F), ex(X, F1)) :- !, logform_to_nnf(~F, F1).
logform_to_nnf(~all2(X, F), ex2(X, F1)) :- !, logform_to_nnf(~F, F1).
logform_to_nnf(~alla(X, F), exa(X, F1)) :- !, logform_to_nnf(~F, F1).
logform_to_nnf((F -> G), FG1) :- !, logform_to_nnf((~F;G), FG1).
logform_to_nnf((F <- G), FG1) :- !, logform_to_nnf((F;~G), FG1).
logform_to_nnf((F <-> G), FG1) :- !, logform_to_nnf(((~F;G),(F;~G)), FG1).
logform_to_nnf(F, F1) :-
	functor(F, Op, 1),
	logop_unary(Op),
	!,
	arg(1, F, F2),
	logform_to_nnf(F2, F3),
	F1 =.. [Op,F3].
logform_to_nnf(FG, FG1) :-
	functor(FG, Op, 2),
	logop_binary(Op),
	!,
	arg(1, FG, F),
	logform_to_nnf(F, F1),
	arg(2, FG, G),
	logform_to_nnf(G, G1),
	FG1 =.. [Op,F1,G1].
logform_to_nnf(F, F1) :-
	functor(F, Op, 2),
	logop_quantifier(Op),
	!,
	arg(1, F, X),
	arg(2, F, F2),
	logform_to_nnf(F2, F3),
	F1 =.. [Op,X,F3].
logform_to_nnf(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- flag(logform_p_counter, _, 1).
:- flag(logform_x_counter, _, 1).
:- flag(logform_f_counter, _, 1).
:- flag(logform_sk_counter, _, 1).

logform_set_p_counter(N) :-
	flag(logform_p_counter, _, N).

logform_set_x_counter(N) :-
	flag(logform_x_counter, _, N).

logform_set_sk_counter(N) :-
	flag(logform_sk_counter, _, N).

logform_reset_counters :-
	logform_set_p_counter(1),
	logform_set_x_counter(1),
	logform_set_sk_counter(1).

logform_gen_predicate(P) :-
	flag_inc(logform_p_counter, N),
	concat_atom(['$p', N], P).

logform_gen_function(X) :-
	flag_inc(logform_x_counter, N),
	concat_atom(['$x', N], X).

logform_gen_symbol(X) :-
	%% for use as predicate or function
	flag_inc(logform_x_counter, N),
	concat_atom(['$x', N], X).

logform_gen_skolem_functor(X) :-
	flag_inc(logform_sk_counter, N),
	concat_atom([sk, N], X).

logform_is_skolem_functor(F) :-
	atom(F),
	sub_atom(F,0,_,_,sk).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%
%%%% Rename quantified predicates and variables with fresh symbols or fresh
%%%% Prolog variables. These are actually "globally" fresh, not just in
%%%% the context of the given formula.
%%%%
%%%% Can be used to convert between Prolog variables and
%%%% symbols (i.e. Prolog atoms) as variables. If second-order variables
%%%% should be Prolog variables, a suitable formula format must be used,
%%%% e.g. tform.
%%%%
%%%% Does not instantiate Prolog variables in the input. Note that
%%%% unquantified Prolog variables in the input are retained in the output.
%%%%
%%%% 
logform_clean_vars(F, F1) :-
	logform_process_subforms(F, clean_vars_to_symbols, F1).
%%%%
logform_clean_vars_to_symbols(F, F1) :-
	logform_clean_vars(F, F1).
%%%%
logform_clean_vars_to_prolog_vars(F, F1) :-
	logform_process_subforms(F, clean_vars_to_prolog, F1).

clean_vars_to_symbols(F, F1) :-
	clean_vars(F, to_symbols, F1).

clean_vars_to_prolog(F, F1) :-
	clean_vars(F, to_prolog_vars, F1).

clean_vars(ex2(X, F), Mode, ex2(X1, F1)) :-
	!,
	logupon_list(X, X2),
	map_fresh_predicates(Mode, X2, X1, Map),
	logform_rename_free_predicates(F, Map, pn, F1).
clean_vars(all2(X, F), Mode, all2(X1, F1)) :-
	!,
	logupon_list(X, X2),
	map_fresh_predicates(Mode, X2, X1, Map),
	logform_rename_free_predicates(F, Map, pn, F1).
clean_vars(ex(X, F), Mode, ex(X1, F1)) :-
	!,
	logupon_list(X, X2),
	map_fresh_functions(Mode, X2, X1, Map),
	logform_rename_free_functions(F, Map, F1).
clean_vars(all(X, F), Mode, all(X1, F1)) :-
	!,
	logupon_list(X, X2),
	map_fresh_functions(Mode, X2, X1, Map),
	logform_rename_free_functions(F, Map, F1).
clean_vars(F, _, F).

map_fresh_predicates(to_symbols, A, B, C) :-
	map_fresh_predicates(A, B, C).
map_fresh_predicates(to_prolog_vars, A, B, C) :-
	map_fresh_predicates_to_prolog_vars(A, B, C).

map_fresh_functions(to_symbols, A, B, C) :-
	map_fresh_functions(A, B, C).
map_fresh_functions(to_prolog_vars, A, B, C) :-
	map_fresh_functions_to_prolog_vars(A, B, C).

map_fresh_predicates([X|Xs], [X1|Ys], [X-X1|Xs1]) :-
	logform_gen_predicate(X1),
	map_fresh_predicates(Xs, Ys, Xs1).
map_fresh_predicates([], [], []).

map_fresh_predicates_to_prolog_vars([X|Xs], [X1|Ys], [X-X1|Xs1]) :-
	map_fresh_predicates_to_prolog_vars(Xs, Ys, Xs1).
map_fresh_predicates_to_prolog_vars([], [], []).

map_fresh_functions([X|Xs], [X1|Ys], [X-X1|Xs1]) :-
	logform_gen_function(X1),
	map_fresh_functions(Xs, Ys, Xs1).
map_fresh_functions([], [], []).

map_fresh_functions_to_prolog_vars([X|Xs], [X1|Ys], [X-X1|Xs1]) :-
	map_fresh_functions_to_prolog_vars(Xs, Ys, Xs1).
map_fresh_functions_to_prolog_vars([], [], []).

logform_rename_free_predicates(F, FromToMap, Pol, F1) :-
	( Pol = pn ->
	  logform_process_subforms_with_bindings(F,
						 mrf_pn(FromToMap),
						 F1)
	; logform_process_subforms_with_polarity_and_bindings(F,
							      mrf(FromToMap, Pol),
							      F1)
	).

logform_rename_free_functions(F, FromToMap, F1) :-
 	logform_process_subforms_with_bindings(F, mrft(FromToMap), F1).

mrft(FromToMap, F, B, F1) :-
	%% "patch" for the t-format, to permit t as variable symbol
	\+ var(F),
	F = P/T,
	functor(T, t, _),
	!,
	T =.. [t|Args],
	map_mrft_1([P|Args], FromToMap, B, [P1|Args1]),
	T1 =.. [t|Args1],
	F1 = P1/T1.
mrft(FromToMap, F, B, F1) :-
	logform_is_atom(F),
	!,
	F =.. [P|Args],
	map_mrft_1(Args, FromToMap, B, Args1),
	F1 =.. [P|Args1].
mrft(_, F, _, F).

mrft_1(X, Map, B, X1) :-
	%% allow prolog variables and compound terms in the role of logic
	%% variables
	\+ logform_bindings_memberchk(X, B),
	lookup(X, Map, X1),
	!.
mrft_1(X, _, _, X) :-
	var(X),
	!.
mrft_1(F, Map, B, F1) :-
	F =.. [X|Args],
	map_mrft_1(Args, Map, B, Args1),
	( \+ logform_bindings_memberchk(X, B), lookup(X, Map, X1) ->
	  true
	; X1 = X
	),
	( Args = [] ->
	  F1 = X1
	; F1 =.. [X1|Args1]
	).

map_mrft_1([X|Xs], Y1, Y2, [X1|Xs1]) :-
	mrft_1(X, Y1, Y2, X1),
	map_mrft_1(Xs, Y1, Y2, Xs1).
map_mrft_1([], _, _, []).

% 
% mrf_pn(FromToMap, F, B, F1) :-
% 	%% allow prolog variables and compound terms in the role of predicate
% 	%% variables (maybe useful for atom forgetting)
% 	logform_is_atom(F),
% 	\+ logform_bindings_memberchk(F, B),
% 	lookup(F, FromToMap, F1),
% 	!.
mrf_pn(FromToMap, Pred/T, B, Pred1/T) :-
	%% Support for tform syntax
	!,
	( \+ logform_bindings_memberchk(Pred, B),
 	  lookup(Pred, FromToMap, Pred1) ->
	  true
	; Pred1 = Pred
	).
mrf_pn(FromToMap, F, B, F1) :-
	logform_is_atom(F),
	functor(F, Pred, _),
	\+ logform_bindings_memberchk(Pred, B),
	lookup(Pred, FromToMap, Pred1),
	!,
	F =.. [_|Args],
	F1 =.. [Pred1|Args].
mrf_pn(_, F, _, F).


mrf(FromToMap, Pol, Pred/T, Pol, B, Pred1/T) :-
	%% Support for tform syntax
	!,
	( \+ logform_bindings_memberchk(Pred, B),
 	  lookup(Pred, FromToMap, Pred1) ->
	  true
	; Pred1 = Pred
	).
mrf(FromToMap, Pol, F, Pol, B, F1) :-
	logform_is_atom(F),
	functor(F, Pred, _),
	\+ logform_bindings_memberchk(Pred, B),
	lookup(Pred, FromToMap, Pred1),
	!,
	F =.. [_|Args],
	F1 =.. [Pred1|Args].
mrf(_, _, F, _, _, F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lookup(X, Map, Y) :-
	member(X1-Y, Map),
	X == X1,
	!.

logform_bindings_memberchk(X, [Y|_]) :- X == Y, !.
logform_bindings_memberchk(X, [_|Y]) :- logform_bindings_memberchk(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_gather_quantifiers(F, F1) :-
	logform_process_subforms(F, gather_quants_1, F1).

gather_quants_1(F, F1) :-
	functor(F, Q, 2),
	logop_quantifier(Q),
	arg(2, F, F2),
	functor(F2, Q, 2),
	!,
	arg(1, F, X1),
	arg(1, F2, X2),
	logupon_list(X1, X1s),
	logupon_list(X2, X2s),
	append(X1s, X2s, Xs),
	arg(2, F2, F3),
	( Xs = [] ->
	  F1 = F3
	; Xs = [X] ->
	  F1 =.. [Q,X,F3]
	; F1 =.. [Q,Xs,F3]
	).
gather_quants_1(F, F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%% Combination predicates that perform very cheap simplifications. If the
%%%% input is in NNF, so is the output.
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


logform_negate(~F, F) :- !.
logform_negate((F,G), (F1;G1)) :-
	!, logform_negate(F, F1), logform_negate(G, G1).
logform_negate((F;G), (F1,G1)) :-
	!, logform_negate(F, F1), logform_negate(G, G1).
logform_negate((F->G), (F,G1)) :- !, logform_negate(G, G1).
logform_negate((F<-G), (F1,G)) :- !, logform_negate(F, F1).
logform_negate((F <-> ~G),(F<->G)) :- !.
logform_negate((~F <-> G),(F<->G)) :- !.
logform_negate((F <-> G), (F <-> G1)) :- !, logform_negate(G, G1).
logform_negate(all(X, F), ex(X, F1)) :-	!, logform_negate(F, F1).
logform_negate(ex(X, F), all(X, F1)) :-	!, logform_negate(F, F1).
logform_negate(all2(X, F), ex2(X, F1)) :- !, logform_negate(F, F1).
logform_negate(ex2(X, F), all2(X, F1)) :- !, logform_negate(F, F1).
logform_negate(true, false) :- !.
logform_negate(false, true) :- !.
logform_negate(F, ~F).

logform_conjoin(false, _, false) :- !.
logform_conjoin(_, false, false) :- !.
logform_conjoin(true, F, F) :- !.
logform_conjoin(F, true, F) :- !.
logform_conjoin(F, G, F) :- F == G, !.
logform_conjoin(F, ~G, false) :- F == G, !.
logform_conjoin(~F, G, false) :- F == G, !.
logform_conjoin((F , G), H, (F , GH1)) :- !, logform_conjoin(G, H, GH1).
logform_conjoin(F, G, (F , G)) :- !.

logform_disjoin(true, _, true) :- !.
logform_disjoin(_, true, true) :- !.
logform_disjoin(false, F, F) :- !.
logform_disjoin(F, false, F) :- !.
logform_disjoin(F, G, F) :- F == G, !.
logform_disjoin(F, ~G, true) :- F == G, !.
logform_disjoin(~F, G, true) :- F == G, !.
logform_disjoin((F ; G), H, (F ; GH1)) :- !, logform_disjoin(G, H, GH1).
logform_disjoin(F, G, (F ; G)) :- !.

logform_exquant(_, true, true) :- !.
logform_exquant(_, false, false) :- !.
logform_exquant(V, F, F) :- V == [], !.
logform_exquant(V, F, ex(V, F)).

logform_allquant(_, true, true) :- !.
logform_allquant(_, false, false) :- !.
logform_allquant(V, F, F) :- V == [], !.
logform_allquant(V, F, all(V, F)).	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_has_second_order_quantifier(F) :-
	logform_enum_subformulas(F, F1),
	( F1 = ex2(_, _) -> true
	; F1 = all2(_, _) -> true
	),
	!.

logform_has_first_order_quantifier(F) :-
	logform_enum_subformulas(F, F1),
	( F1 = ex(_, _) -> true
	; F1 = all(_, _) -> true
	),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logform_size(F, N) :-
	flag(tmp_size, _, 0),
	( logform_enum_atoms(F, _),
	  flag_inc(tmp_size, _),
	  fail
	; true
	),
	flag(tmp_size, N, N).
