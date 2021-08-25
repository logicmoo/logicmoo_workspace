
:- dynamic(ptemplate/1),
   dynamic(template/1),
   dynamic(inf_rule/2),
   dynamic(ht_flag/2),
   dynamic(ground_clause_set/0),
   dynamic(initial_interpretation/1),
   dynamic(sorted/0),
   dynamic(constant/1),
   dynamic(pconstant/1),
   dynamic(fun_sym_sort/1),
   dynamic(pred_sym/1),
   dynamic(component/1), % for diagnosis applications
   dynamic(fun_sym/1).

:- global_op( 1200, xfx, '<-').
:- global_op( 1200, xf, '<-').
:- global_op( 1040, xfy, '<->').
:- global_op( 1200, yfx, '#').

:- setval(input_mode, regular). %% can be one of { regular, theory, prolog }
:- setval(expand_read, true). %% expand the read(.) here or pass the
                               %% read statement?

init_parser :-
	retract_preds_dynamic,
	retract_all(ptemplate(_)),
	retract_all(ht_flag(_,_)),
	retract_all(sorted),
	retract_all(template(_)),
	retract_all(initial_interpretation(_)),
	retract_all(constant(_)),
	retract_all(pconstant(_)),
	retract_all(fun_sym(_)),
	retract_all(component(_)),
	retract_all(fun_sym_sort(_)),
	retract_all(ground_clause_set),
	assert(ground_clause_set),
	retract_all(inf_rule(_,_)).

consult_file(File) :-

	(string(File) -> atom_string(TmeFilename,File)
    ;
	concat_atoms(File, '.tme', TmeFilename) ),

	open(TmeFilename, read, TmeFile),
	
%	write('% Reading '), write(TmeFilename), writeln('...'), nl,
	repeat,
	myread(TmeFile, Term),
%	write('% '), writeln(Term),
	translate_cut(Term),
	Term == end_of_file,
	!,

	(constant(_C) ; assert(constant(any))),

	close(TmeFile),
	
%	write('% '), write(TmeFilename), writeln(' done.'), nl,
        true.

translate_cut(Term) :- translate(Term), !.

translate(end_of_file) :- !.

translate(Term) :- 
	%% the term is a comment?
	Term =.. [Unary],
	name(Unary,[37|_]), %% 37 -> ascii '%'
	assert(oi_clause(Term,comment)).
	

%translate(mission_flag(Name, Value) ) :- !, flag(Name, Value).


translate((Op : Decl)) :- 
	assert(oi_clause((Op : Decl),skip)),
	!.

translate(Term) :- 
	( getval(input_mode, prolog) ; 
	  getval(input_mode, theory) ; 
          getval(input_mode, simplify)),
	assert(oi_clause(Term,skip)),
	(Term = end(_) -> 
	    setval(input_mode, regular)
	    ; true),
	!.

% we know that we are in regular input mode

translate(read(File) ) :- 
	%%% BUG - this does currently not work
	getval(expand_read, true) ->
	     consult_file(File)
	 ; assert(oi_clause(read(File),skip)).


translate(initial_interpretation(I)) :- 
	assert(initial_interpretation(I)),
	!.

translate(Term) :- 
	protein_term(Term),
	%% an alternative for specifying initial interpretations:
	(Term = ht_flag(initial_interpretation,I) ->
	    assert(initial_interpretation(I))
	; true),
	assert(oi_clause(Term,skip)),
	!.


translate(Term) :- 

	mission_term(Term), 
	!.



translate( (?- Goal) ) :- !,
	pl_list_con(Goal, GoalList),
	negate_list(GoalList, OI_clause), 
	normalize_assert(OI_clause,query).

% this feature is disabled in parser.pl, since in conflict with pl1-formulas
%translate( (P -> C) ) :- !,
%	pl_list(P, PL),
%	norm_list(PL,PLN),
%	norm_list([C],[CN]),
%	(mydelete((-Goal),PLN,Rest)
%          -> assert(inf_rule((-Goal, Rest -> CN)))
%          ; true).

% not needed
translate( (P -> C) ) :- !,
	pl_list(P, PL),
	norm_list(PL,PLN),
	norm_list([C],[CN]),
	assert(inf_rule(PLN,[CN])).

translate( (Head :- Body) ) :- !,

	pl_list(Head, HeadList), 
	pl_list(Body, BodyList), 

	negate_list(BodyList, NBodyList), 
	append(HeadList, NBodyList, OI_clause),
	normalize_assert(OI_clause,input).

translate( '<-'(Head) ) :- !,

	pl_list(Head, HeadList), 
	normalize_assert(HeadList,input).

translate( '<-'(Head,Body) ) :- !,

	pl_list(Head, HeadList), 
	pl_list(Body, BodyList), 

	negate_list(BodyList, NBodyList), 
	append(HeadList, NBodyList, OI_clause),
	normalize_assert(OI_clause,input).

translate(Clause) :-  % should be ';' or ',' separated list of literals

	pl_list_dis_or_con(Clause, OI_clause), 
	normalize_assert(OI_clause,input).


protein_term(ht_flag(_, _)).
protein_term(protein_flag(_, _)).
protein_term(prolog_term(_, _)).
protein_term(pttp_flag(_, _)).
protein_term(calculus(_)).
protein_term(mod_flag(_,_)).
protein_term(begin(theory)) :-
	setval(input_mode, theory).
protein_term(begin(prolog)) :-
	setval(input_mode, prolog).
protein_term(begin(simplify)) :-
	setval(input_mode, prolog).
% special treatment above
%protein_term(end(_)) :- 
%	setval(input_mode, regular).
% special treatment -> see above
%protein_term( read(_)) :- 
%	getval(expand_read, false).
protein_term( depth_increment(_)).
protein_term( theory_costs(_,_)).
protein_term( prolog_pred(_/_)).
protein_term( scanner_flag(_,_)).
protein_term( lc_flag(_,_)).



mission_term(mission_flag(_, _)).


analyze_clause([]).
analyze_clause([-Lit|Rest]) :- !,
	analyze_lit(Lit), 
	analyze_clause(Rest), !.
analyze_clause([Lit|Rest]) :-
	analyze_lit(Lit), 
	analyze_clause(Rest), !.

analyze_lit(L) :- var(L), !.
analyze_lit(L) :-
	%% this serves for declarations only in sorted specification
	%% and hence can be skipped
	functor(L,(':'),_N), !.
analyze_lit(L) :-
	functor(L,F,N),
	(pred_sym(F / N) ; assert(pred_sym(F / N))),
	(N = 0  ->
	    (pconstant(F) ; assert(pconstant(F)))
	; template_args(N,TemplateArgs),
	  Template =.. [F|TemplateArgs],
	  (ptemplate(Template) ; assert(ptemplate(Template)))),
	%% remember components for diagnosis:
	((L = ab(Comp) ; L = high(Comp,_Line)) ->
	    (component(Comp) ; assert(component(Comp)))
	 ;  true),
	L =.. [_P|Args],
	analyze_term_list(Args).

analyze_term_list([]).
analyze_term_list([T|R]) :-
        analyze_term(T),
	analyze_term_list(R).

analyze_term(X) :- var(X), !,
	retract_all(ground_clause_set).
analyze_term((T:Sort)) :- !,
	%% we have a sorted specification
	(sorted ; asserta(sorted)),
	(nonvar(T) ->
	    functor(T,F,_N),
	    (fun_sym_sort(F:Sort) ; assert(fun_sym_sort(F:Sort)))
	; true),
	%% we analyze only the term, but not the sort
	analyze_term(T).
analyze_term(T) :- 
	functor(T,F,N),
	(N = 0 ->
	    (constant(F) ; assert(constant(F)))
         ;  template_args(N,TemplateArgs),
	    Template =.. [F|TemplateArgs],
	    (fun_sym(F / N) ; assert(fun_sym(F / N))),
	    (template(Template) ; assert(template(Template)))),
	T =.. [_F|Args],
	analyze_term_list(Args).
	     

template_args(0,[]).
template_args(1,[_A_1]).
template_args(2,[_A_1,_A_2]).
template_args(3,[_A_1,_A_2,_A_3]).
template_args(4,[_A_1,_A_2,_A_3,_A_4]).
template_args(5,[_A_1,_A_2,_A_3,_A_4,_A_5]).
template_args(6,[_A_1,_A_2,_A_3,_A_4,_A_5,_A_6]).
template_args(7,[_A_1,_A_2,_A_3,_A_4,_A_5,_A_6,_A_7]).
template_args(8,[_A_1,_A_2,_A_3,_A_4,_A_5,_A_6,_A_7,_A_8]).


sorted_template_args(0,[]).
sorted_template_args(1,[_A_1:_S_1]).
sorted_template_args(2,[_A_1:_S_1,_A_2:_S_2]).
sorted_template_args(3,[_A_1:_S_1,_A_2:_S_2,_A_3:_S_3]).
sorted_template_args(4,[_A_1:_S_1,_A_2:_S_2,_A_3:_S_3,_A_4:_S_4]).
sorted_template_args(5,[_A_1:_S_1,_A_2:_S_2,_A_3:_S_3,_A_4:_S_4,_A_5:_S_5]).
sorted_template_args(6,[_A_1:_S_1,_A_2:_S_2,_A_3:_S_3,_A_4:_S_4,_A_5:_S_5,_A_6:_S_6]).
sorted_template_args(7,[_A_1:_S_1,_A_2:_S_2,_A_3:_S_3,_A_4:_S_4,_A_5:_S_5,_A_6:_S_6,
	_A_7:_S_7]).
sorted_template_args(8,[_A_1:_S_1,_A_2:_S_2,_A_3:_S_3,_A_4:_S_4,_A_5:_S_5,_A_6:_S_6,
	_A_7:_S_7,_A_8:_S_8]).


p_norm_clause([],[]).
p_norm_clause([-false|_R],[true]).
p_norm_clause([~false|_R],[true]).
p_norm_clause([true|_R],[true]).
p_norm_clause([false|L],LR) :- !, p_norm_clause(L,LR).
p_norm_clause([-true|L],LR) :- !, p_norm_clause(L,LR).
p_norm_clause([~true|L],LR) :- !, p_norm_clause(L,LR).
p_norm_clause([~K|L],[-K|LR]) :- p_norm_clause(L,LR), !.
p_norm_clause([K|L],[K|LR]) :- p_norm_clause(L,LR), !.

normalize_assert(Clause,Type) :-
        p_norm_clause(Clause,NC),
	analyze_clause(NC), 
	(ht_flag(polarity_switch,on) -> 
	    polarity_switch(NC,NCP)
	  ; NCP = NC),
	(initial_interpretation(I) -> 
	    interpretation_switch(NCP,I,NCPI)
	  ; NCPI = [NCP]),
	normalize_assert_(NCPI,Type).


normalize_assert_([],_Type).
normalize_assert_([One|More],Type) :-
	assert(oi_clause(One,Type)),
	normalize_assert_(More,Type).

polarity_switch([],[]).
polarity_switch([L|R],[L1|R1]) :- negate(L,L1), polarity_switch(R,R1).

premisses( (Prem, More), Norm) :- !,

	positive_literal(Prem, Next, Norm),
	premisses(More, Next).

premisses(Prem, Norm) :- positive_literal(Prem, [], Norm).


conclusio( (Conc, More), Pres, Norm) :- !,

	negative_literal(Conc, Next, Norm),
	conclusio(More, Pres, Next).

conclusio( (Conc; More), Pres, Norm) :- !,

	negative_literal(Conc, Next, Norm),
	conclusio(More, Pres, Next).

conclusio(Conc, Pres, Norm) :- negative_literal(Conc, Pres, Norm).


positive_literal(true, Norm, Norm) :- !.

positive_literal(~ Lit, More, [- Lit | More]) :- !.

positive_literal(Lit, More, [Lit | More]).


negative_literal(false, Norm, Norm) :- !.

negative_literal(~ Lit, More, [Lit | More]) :- !.

negative_literal(Lit, More, [Neg | More]) :- negate(Lit, Neg).


retract_preds_dynamic :-
	pred_sym(P/N),
%	template_args(N,L),
%	Pred =.. [P|L],
%	retract_all(Pred), 
	retract(pred_sym(P/N)),
	fail.
retract_preds_dynamic.

make_preds_dynamic :-
	pred_sym(P/N),
	template_args(N,L),
	Pred =.. [P|L],
	translate((~Pred, Pred -> false)),
	fail.
make_preds_dynamic.


