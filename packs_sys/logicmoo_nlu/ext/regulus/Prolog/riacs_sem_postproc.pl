:- ensure_loaded('$REGULUS/PrologLib/compatibility').

:- module(riacs_sem_postproc,
	[add_variables/2,
	 scope_variables/2,
	 simplify_scoped_form/2
	 ]
    ).


:- use_module('$REGULUS/PrologLib/utilities').

:- use_module(library(lists)).
:- use_module(library(terms)).

/*

Predicates for turning a logical form produced using the RIACS semantics into
a scoped logical form. There are two top-level predicates:

   add_variables(+RiacsLF, -LFWithVars)

Adds variables to a RIACS LF. You should provide definitions for some of all of the following predicates:

lf_consts:modal_tense_and_aspect/2

e.g.

modal_tense_and_aspect(can).
modal_tense_and_aspect(imperative).

lf_consts:conj/2

e.g.

conj(and, exists_and).

   scope_variables(+LFWithVars, -ScopedLF)

Turns the output of add_variables/2 into a scoped logical form.

So far, both of these predicates are very simple. Here's an example, from the SHRD2 domain:

"put the red block in the box"

             LF: [[imp, 
                   form(imperative, 
                        [[put,term(pro,you,[]),term(the_sing,block,[[color,red]])], 
                         [in_loc,term(the_sing,box,[])]])]]
     LFWithVars: [[imp, 
                   scoping_unit([modal, imperative, 
                                 term(event_exists, A, 
                                      [[put, A, term(pro,B,[[you,B]]), 
                                        term(the_sing,C,[[block,C],[color,C,red]])],
                                       [in_loc,A,term(the_sing,D,[[box,D]])]])])]]
       ScopedLF: [[imp, 
                   quant(pro, A, [[you,A]], 
                         quant(the_sing, B, [[block,B],[color,B,red]], 
                               quant(the_sing, C, [[box,C]], 
                                     imperative(quant(event_exists, D, 
                                                      [[put,D,A,B],[in_loc,D,C]], true)))))]]


*/

%======================================================================

add_variables(Atom, Atom) :-
	atomic(Atom),
	!.
add_variables(Var, Var) :-
	var(Var),
	!.
add_variables(form(TenseAndAspect, [MainVerbRep | Mods]),
	      Result) :-
	add_variables_to_main_verb_rep(MainVerbRep, EventVar, MainVerbRep1),
	add_variables_to_mods(Mods, EventVar, Mods1),
	(   ( current_predicate(lf_consts:modal_tense_and_aspect/1), lf_consts:modal_tense_and_aspect(TenseAndAspect) ) ->
	    Result = scoping_unit([modal, TenseAndAspect, term(event_exists, EventVar, [MainVerbRep1 | Mods1])])
	;
	    otherwise ->
	    add_variables_to_tense_rep(TenseAndAspect, EventVar, Tense1),
	    Result = scoping_unit(term(event_exists, EventVar, [MainVerbRep1, Tense1 | Mods1]))
	),
	!.
add_variables(term(Conj, Conjuncts, Mods),
	      term(ConjQuant, Var, Body)) :-
	current_predicate(lf_consts:conj/2), lf_consts:conj(Conj, ConjQuant),
	add_variables(Conjuncts, Conjuncts1),
	add_variables_to_mods(Mods, Var, Mods1),
	Body = [[Conj, Var | Conjuncts1] | Mods1],
	!.
add_variables(term(Quant, Head, Mods),
	      term(Quant, Var, [Head1 | Mods1])) :-
	add_variables_to_head_rep(Head, Var, Head1),
	add_variables_to_mods(Mods, Var, Mods1),
	!.
add_variables(Term, Term1) :-
	compound(Term),
	functor(Term, F, N),
	functor(Term1, F, N),
	add_variables_args(N, Term, Term1),
	!.
add_variables(X, Y) :-
	format('~N*** Error: bad call: ~w~n', [add_variables(X, Y)]),
	fail.

add_variables_args(I, _Term, _Term1) :-
	I < 1,
	!.
add_variables_args(I, Term, Term1) :-
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	add_variables(Arg, Arg1),
	I1 is I - 1,
	!,
	add_variables_args(I1, Term, Term1).

add_variables_to_main_verb_rep(MainVerbRep, EventVar, MainVerbRep1) :-
	MainVerbRep = [Verb | Args],
	MainVerbRep1 = [Verb, EventVar | Args1],
	add_variables(Args, Args1),
	!.
add_variables_to_main_verb_rep(MainVerbRep, EventVar, MainVerbRep1) :-
	format('~N*** Error: bad call: ~w~n', [add_variables_to_main_verb_rep(MainVerbRep, EventVar, MainVerbRep1)]),
	fail.

add_variables_to_tense_rep(Tense, EventVar, [tense, EventVar, Tense]) :-
	!.

add_variables_to_mods([], _Var, []).
add_variables_to_mods([F | R], Var, [F1 | R1]) :-
	add_variables_to_mod(F, Var, F1),
	!,
	add_variables_to_mods(R, Var, R1).

% [lambda,y,form(present,[[support,[y],term(a,pyramid,[])]])]
add_variables_to_mod(Mod, Var, Mod1) :-
	Mod = [lambda, LambdaVar, Body],
	substitute_in_term(Body, [LambdaVar], Var, Body1),
	add_variables(Body1, Body2),
	Mod1 = Body2,
	!.
add_variables_to_mod(Mod, Var, Mod1) :-
	Mod = [Rel | Args],
	Mod1 = [Rel, Var | Args1],
	add_variables(Args, Args1),
	!.
add_variables_to_mod(Mod, Var, Mod1) :-
	format('~N*** Error: bad call: ~w~n', [add_variables_to_mod(Mod, Var, Mod1)]),
	fail.

add_variables_to_head_rep(Head, Var, Head1) :-
	Head1 = [Head, Var].


%======================================================================

/*

                 [[imp, 
                   scoping_unit([modal, imperative, 
                                 term(event_exists, A, 
                                      [[pick_up, A, term(pro,B,[[you,B]]), 
                                        term(a,C,[[block,C],[size,C,big],[color,C,red]]), up]])])]]
		 

                 [[imp, 
                   scoping_unit([modal, imperative, 
                                 term(event_exists, A, 
                                      [[put, A, term(pro,B,[[you,B]]), 
                                        term(a,C,[[one,C],[size,C,small]])],
                                       [onto_loc, A, 
                                        term(the_sing, D, 
                                             [[cube,D], [color,D,green], 
                                              scoping_unit(term(event_exists, E, 
                                                                [[support, E, D, 
                                                                  term(a,F,[[pyramid,F]])],
                                                                 [tense,E,present]]))])]])])]]	 

*/

scope_variables(Var, Var) :-
	var(Var),
	!.
scope_variables(Atom, Atom) :-
	atomic(Atom),
	!.
scope_variables(scoping_unit(Body), Result) :-
	build_quantifier_store(Body, Body1, Store-[]),
	order_quantifiers(Store, OrderedQuants),
	build_scoped_form(OrderedQuants, Body1, Result),
	!.
scope_variables(Term, Term1) :-
	compound(Term),
	functor(Term, F, N),
	functor(Term1, F, N),
	scope_variables_args(N, Term, Term1),
	!.
scope_variables(Term, Term1) :-
	format('~N*** Error: bad call: ~w~n', [scope_variables(Term, Term1)]),
	fail.

scope_variables_args(I, _Term, _Term1) :-
	I < 1,
	!.
scope_variables_args(I, Term, Term1) :-
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	scope_variables(Arg, Arg1),
	I1 is I - 1,
	!,
	scope_variables_args(I1, Term, Term1).

%----------------------------------------------------------------------

build_quantifier_store(Var, Var, Store-Store) :-
	var(Var),
	!.
build_quantifier_store(Atom, Atom, Store-Store) :-
	atomic(Atom),
	!.
build_quantifier_store(scoping_unit(Body), Result, Store-Store) :-
	scope_variables(scoping_unit(Body), Result),
	!.
build_quantifier_store(term(event_exists, Var, Body), quant(event_exists, Var, Body1, true), StoreIn-StoreOut) :-
	build_quantifier_store(Body, Body1, StoreIn-StoreOut),
	!.
build_quantifier_store(term(Quant, Var, Body), Var, StoreIn-StoreOut) :-
	Quant \== event_exists,
	build_quantifier_store(Body, Body1, StoreNext-StoreOut),
	StoreIn = [quant(Quant, Var, Body1) | StoreNext],
	!.
build_quantifier_store(Form, Body1, StoreIn-StoreOut) :-
	safe_subsumes_chk([modal, Modal, Body], Form),
	Form = [modal, Modal, Body],
	build_quantifier_store(Body, Body1, StoreNext-StoreOut),
	StoreIn = [modal(Modal) | StoreNext],
	!.
build_quantifier_store([PolarityItem | Body], Body1, StoreIn-StoreOut) :-
	safe_subsumes_chk([polarity, _, Polarity], PolarityItem),
	PolarityItem = [polarity, _, Polarity],
	build_quantifier_store(Body, Body1, StoreNext-StoreOut),
	StoreIn = [polarity(Polarity) | StoreNext],
	!.
build_quantifier_store(Term, Term1, StoreIn-StoreOut) :-
	compound(Term),
	functor(Term, F, N),
	functor(Term1, F, N),
	build_quantifier_store_args(N, Term, Term1, StoreIn-StoreOut),
	!.
build_quantifier_store(Term, Term1, Store) :-
	format('~N*** Error: bad call: ~w~n', [build_quantifier_store(Term, Term1, Store)]),
	fail.

build_quantifier_store_args(I, _Term, _Term1, StoreIn-StoreIn) :-
	I < 1,
	!.
build_quantifier_store_args(I, Term, Term1, StoreIn-StoreOut) :-
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	build_quantifier_store(Arg, Arg1, StoreIn-StoreNext),
	I1 is I - 1,
	!,
	build_quantifier_store_args(I1, Term, Term1, StoreNext-StoreOut).

%----------------------------------------------------------------------

% Temporary def
order_quantifiers(Store, OrderedQuants) :-
	Store = OrderedQuants,
	!.
order_quantifiers(Store, OrderedQuants) :-
	format('~N*** Error: bad call: ~w~n', [order_quantifiers(Store, OrderedQuants)]),
	fail.

%----------------------------------------------------------------------

build_scoped_form([], Body, Body) :-
	!.
build_scoped_form([F | R], BodyIn, BodyOut) :-
	apply_quantifier_store_element(F, BodyIn, BodyNext),
	!,
	build_scoped_form(R, BodyNext, BodyOut).
build_scoped_form(Store, Body, Body1) :-
	format('~N*** Error: bad call: ~w~n', [build_scoped_form(Store, Body, Body1)]),
	fail.

apply_quantifier_store_element(quant(Quant, Var, Restriction), Body, quant(Quant, Var, Restriction, Body)) :-
	!.
apply_quantifier_store_element(modal(Modal), Body, BodyOut) :-
	BodyOut =.. [Modal, Body],
	!.
apply_quantifier_store_element(polarity(negative), Body, BodyOut) :-
	BodyOut = not(Body),
	!.
apply_quantifier_store_element(Quant, Body, BodyOut) :-
	format('~N*** Error: bad call: ~w~n', [apply_quantifier_store_element(Quant, Body, BodyOut)]),
	fail.

%======================================================================

simplify_scoped_form(V, V) :-
	var(V),
	!.
simplify_scoped_form(A, A) :-
	atomic(A),
	!.
simplify_scoped_form(quant(Quant, Var, Restriction, Body),
		     Result) :-
	simplify_quant(Quant, Quant1),
	simplify_scoped_form_restriction(Restriction, Restriction1),
	simplify_scoped_form(Body, Body1),
	(   contains_var(Var, [Restriction1, Body]) ->
	    Result = quant(Quant1, Var, Restriction1, Body1)
	;
	    otherwise ->
	    Result = Body1
	),
	!.
simplify_scoped_form(Term, Term1) :-
	compound(Term),
	functor(Term, F, N),
	functor(Term1, F, N),
	simplify_scoped_form_args(N, Term, Term1),
	!.
simplify_scoped_form(Term, Term1) :-
	format('~N*** Error: bad call: ~w~n', [simplify_scoped_form(Term, Term1)]),
	fail.

simplify_scoped_form_args(I, _Term, _Term1) :-
	I < 1,
	!.
simplify_scoped_form_args(I, Term, Term1) :-
	arg(I, Term, Arg),
	arg(I, Term1, Arg1),
	simplify_scoped_form(Arg, Arg1),
	I1 is I - 1,
	!,
	simplify_scoped_form_args(I1, Term, Term1).

%----------------------------------------------------------------------

simplify_scoped_form_restriction(RestrictionIn, RestrictionOut) :-
	simplify_scoped_form_restriction1(RestrictionIn, RestrictionNext),
	(   RestrictionNext = [] ->
	    RestrictionOut = true
	;
	    otherwise ->
	    RestrictionOut = RestrictionNext
	).

simplify_scoped_form_restriction1([true | R], Result) :-
	simplify_scoped_form_restriction1(R, Result),
	!.
simplify_scoped_form_restriction1([F | R], [F1 | R1]) :-
	simplify_scoped_form(F, F1),
	!,
	simplify_scoped_form_restriction1(R, R1).
simplify_scoped_form_restriction1(Other, Other).

%----------------------------------------------------------------------

simplify_quant(Quant, Quant1) :-
	current_predicate(lf_consts:quant/2),
	lf_consts:quant(Quant, Quant1),
	!.
simplify_quant(event_exists, exist).
simplify_quant(exists_and, exist).
simplify_quant(pro, def_sing).
simplify_quant(Quant, Quant).

%----------------------------------------------------------------------


