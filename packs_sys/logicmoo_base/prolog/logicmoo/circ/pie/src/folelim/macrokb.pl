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

:- module(macrokb,
	  [mac_install/1,
	   mac_install/0,
	   mac_list/1,
	   mac_uninstall/1,
	   mac_has_def/1,
	   mac_expand/2,
	   mac_expand_toplevel/2,
	   mac_expand_step/2,
	   mac_to_tptp/2,
	   tptp_to_mac/2,
	   canonicalize_def/11]).

:- use_module(pplatex(brief_syntax)).
:- use_module(tptpio).
:- use_module(logop_fol).
:- use_module(formutils_fol).
:- use_module(prep_fol).
:- use_module(varfun_syntax).
:- use_module(swilib(options)).
:- use_module(swilib(err)).
:- use_module(swilib(fromonto)).
:- use_module(swilib(sysdep)).

:- op(1170, xfx, user:'::').  %% larger than <->
:- op(1165, xfx, user:'::-').
:- multifile(user:'::'/2).
				% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic expansion_method/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mac_install(File) :-
	info(10, 'Installing PIE definitions from ~q', [File]),
	from_file( ( repeat,
		     read_term(current_input, Term,
			       [variable_names(Varnames)]),
		     ( Term == end_of_file ->
		       !
		     ; Term = (L :: V) ->
		       mac_install_def((L :: V), Varnames, File),
		       fail
		     ; % format('Skipping term: ~q~n', [Term]),
		       fail
		     )
		   ; true
		   ),
		   File ).

mac_install :-
	findall(S, ( clause((_ :: _), true, Ref),
		     clause_property(Ref, source(S) )),
		Sources),
	rm_duplicates(Sources, Sources1),
	( member(Source, Sources1),
	  mac_install(Source),
	  fail
	; true
	).

rm_duplicates(X, X1) :-
	reverse(X, X2),
	rmd(X2, X3),
	reverse(X3, X1).

rmd([X|Xs], Xs1) :-
	memberchk(X, Xs),
	!,
	rmd(Xs, Xs1).
rmd([X|Xs], [X|Xs1]) :-
	rmd(Xs, Xs1).
rmd([], []).

%%	
%% The variable names are not available via clause_property but might be
%% required for some types of input conversion, hence this has been
%% reimplemented based on reading the source files.
%% 	
%% mac_install :-
%% 	%% the initialization wrapper is required here for SWI >= 7.4
%% 	initialization(( clause((L :: V), true, Ref),
%% 			 clause_property(Ref, source(Source)),
%% 			 mac_install_def((L :: V), [], Source),
%% 			 fail
%% 		       ; true
%% 		       )).
%%

mac_uninstall(DispPattern) :-
	%% *** ::/2?
	%% retractall( expansion_method(_, DispPattern, _) ).
	( clause(expansion_method(_, DispPattern1, _), _, Ref),
	  subsumes_chk(DispPattern, DispPattern1),
	  erase(Ref),
	  fail
	; true
	).

mac_list(DispPattern) :-
	clause(expansion_method(_, DispPattern, _), _).

mac_has_def(F) :-
	\+ \+ expansion_method(F, _, _).

mac_install_def(Stmt, Varnames, Source) :-
	info(100, 'Installing: ~q from ~q', [Stmt, Source]),
	canonicalize_def(Stmt, Source, Type, Varnames,
			 Pattern, PatternCode, DispPattern,
			 ParamTerm, ParamTermType, _ParamMap, _Options),
	( Type = def ->
	  install_macro(Pattern, PatternCode, DispPattern,
			ParamTerm, ParamTermType)
	; true
	).

install_macro(Pattern, PatternCode, DispPattern, Term, TermType) :-
	mac_uninstall(DispPattern),
	( TermType = instance ->
	  ( match((E ::- Body), Term) ->
	    M = expansion_method(Pattern, DispPattern, instance_rule(E, Body))
	  ; M = expansion_method(Pattern, DispPattern, instance(Term))
	  )
	; ( match((E ::- Body), Term) ->
	    M = expansion_method(Pattern,
				 DispPattern,
				 instance_rule_with_varfunctor(E, Body))
	  ; M = expansion_method(Pattern, DispPattern,
				 instance_with_varfunctor(Term))
	  )
	),
	( PatternCode = true ->
	  assert(M)
	; assert((M :- PatternCode))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kblabel_options(L, Options) :-
	functor(L, _, 2),
	!,
	arg(2, L, Options).
kblabel_options(_, []).

kblabel_type(L, Type) :-
	functor(L, Type, _).

kblabel_pattern_and_params(L, Pattern, Params) :-
	arg(1, L, ParPattern),
	( ParPattern = Params1-Pattern ->
	  term_variables(Pattern, Vars),
	  ( Params1 = [] -> Params = Vars
	  ; Params1 = [_|_] -> append(Params1, Vars, Params)
	  ; append([Params1], Vars, Params)
	  )
	; Pattern = ParPattern,
	  term_variables(Pattern, Params)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

canonicalize_def((L :: V), Source, Type, Varnames, Pattern, PatternCode,
		 DisplayedPattern,
		 ParamTerm, ParamTermType, ParamMap, Options) :-
	kblabel_type(L, Type),
	kblabel_options(L, Options),
	from_options(syntax=Syntax, Options, te),
	( Syntax = brief ->
	  default_brief_options(Source, BriefOptions1),
	  from_options(brief_options=BriefOptions, Options, BriefOptions1),
	  brief_to_form(V, [is_defined_elsewhere_callback=mac_has_def,
			    into_compounds=true|BriefOptions], V1)
	; Syntax = te ->
	  V1 = V
	; err('Unsupported syntax for ~q', [L])
	),
	( varfun_mode ->
	  term_to_vfterm(L-V1, Varnames, L2-V2)
	; L2 = L,
	  V2 = V1
	),
	kblabel_pattern_and_params(L2, Pattern1, Params),
	term_to_paramterm(Pattern1-V2,
			  Params,
			  DisplayedPattern-ParamTerm,
			  ParamTermType,
			  ParamMap),
	extract_arithmetic_pattern(DisplayedPattern, Pattern, PatternCode).

term_to_paramterm(Term, [], Term, instance, []) :-
	!.
term_to_paramterm(Term, Params, ParamTerm, TermType, ParamMap) :-
	map_params(Params, ParamMap),
	ttp(Term, ParamMap, ParamTerm, IsPredParam),
	( IsPredParam == true ->
	  TermType = instance_with_varfunctor
	; TermType = instance
	).

map_params([X|Xs], [X-_|Xs1]) :-
	map_params(Xs, Xs1).
map_params([], []).

instantiate_paramterm(pred_paramterms(Params, ParamTerm), Vals, Term) :-
	!,
	copy_term(Params-ParamTerm, Vals-Term1),
	ffp(Term1, Term).
instantiate_paramterm(arg_paramterms(Params, ParamTerm), Vals, Term) :-
	!,
	copy_term(Params-ParamTerm, Vals-Term).

param_map_lookup(X-Y, ParamMap) :-
	member(X1-Y, ParamMap),
	X1 == X,
	!.

ttp(T, ParamMap, T1, _) :-
	param_map_lookup(T-T1, ParamMap),
	!.
ttp(T, ParamMap, T1, true) :-
	compound(T),
	functor(T, F, _),
	param_map_lookup(F-F1, ParamMap),
	!,
	T =.. [_|Args],
	map_ttp(Args, ParamMap, _, Args1),
	T1 = '$varfunctor'(F1, Args1).
ttp(T, ParamMap, T1, IsPredParam) :-
	compound(T),
	!,
	T =.. [F|Args],
	map_ttp(Args, ParamMap, IsPredParam, Args1),
	T1 =.. [F|Args1].
ttp(T, _, T, _).

map_ttp([X|Xs], Y1, IsPredParam, [X1|Xs1]) :-
	ttp(X, Y1, X1, IsPredParam),
	map_ttp(Xs, Y1, IsPredParam, Xs1).
map_ttp([], _, _, []).


ffp('$varfunctor'(F, Args), T) :-
	!,
	T =.. [F|Args].
ffp(T, T1) :-
	compound(T),
	!,
	T =.. [F|Args],
	map_ffp(Args, Args1),
	T1 =.. [F|Args1].
ffp(T, T).

map_ffp([X|Xs], [X1|Xs1]) :-
	ffp(X, X1),
	map_ffp(Xs, Xs1).
map_ffp([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extract_arithmetic_pattern(Term, Term1, Condition) :-
	eat(Term, Term1, [], Calls),
	reverse(Calls, Calls1),
	list_to_conjunction(Calls1, Condition).

eat(T, T, Calls, Calls) :-
	var(T),
	!.
eat(T, T, Calls, Calls) :-
	atomic(T),
	!.
%% more convenient to start induction with 1 as base case? ***
eat((1+X), Y, Calls, [X is Y-1, Y > 1, number(Y)|Calls]) :-
	!.
eat(T, T1, Calls, Calls1) :-
	T =.. [F|Ts],
	map_eat(Ts, Ts1, Calls, Calls1),
	T1 =.. [F|Ts1].

map_eat([T|Ts], [T1|Ts1], Calls, Calls1) :-
	eat(T, T1, Calls, Calls2),
	map_eat(Ts, Ts1, Calls2, Calls1).
map_eat([], [], Calls, Calls).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% Macro Expansion
%%%% 
%%%% - Macros result in formulas, that is, terms with some syntactic
%%%%   restrictions.
%%%% 
%%%% - The syntax of these formula is not fully determined to allow variants
%%%%   of the input syntax. (See "Syntax Specifications" section in this
%%%%   file.)
%%%%
%%%% - Independently of this, several syntaxes for formula input are
%%%%   supported, in particular, aside of the standard syntax "te" also
%%%%   the "brief" syntax.
%%%%
%%%% - Macros expansion is driven by patterns.
%%%% 
%%%% - With each macro a pattern is associated. Patterns of different macros
%%%%   are not allowed to be unifiable.
%%%% 
%%%% - There are several possibilities for the order in which macros are
%%%%   expanded.
%%%%   The following is used for now:
%%%%   - 1. repeatedly expand the outermost term (until it no longer matches a
%%%%        macro pattern)
%%%%     2. expand subterms
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mac_expand(T, T1) :-
	mac_expand_toplevel(T, T2),
	mac_expand_subterms(T2, T1).

mac_expand_subterms(T1, T2) :-
	compound(T1),
	!,
	T1 =.. [F|Ts],
	map_mac_expand(Ts, Ts1),
	T2 =.. [F|Ts1].
mac_expand_subterms(T, T).

map_mac_expand([X|Xs], [X1|Xs1]) :-
	mac_expand(X, X1),
	map_mac_expand(Xs, Xs1).
map_mac_expand([], []).

mac_expand_step(T, T1) :-
	expansion_method(T, _, Method),
	!,
	apply_expansion_method(Method, T1).
mac_expand_step(T, T).

mac_expand_toplevel(T, T1) :-
	expansion_method(T, _, Method),
	!,
	apply_expansion_method(Method, T2),
	mac_expand_toplevel(T2, T1).
mac_expand_toplevel(T, T).

apply_expansion_method(instance(T), T1) :-
	!,
	after_expansion(T, T1).
apply_expansion_method(instance_with_varfunctor(T), T1) :-
	!,
	ffp(T, T2),
	after_expansion(T2, T1).
apply_expansion_method(instance_rule(T, Body), T1) :-
	!,
	call(Body),
	after_expansion(T, T1).
%% ???
apply_expansion_method(instance_rule_with_varfunctor(T, Body), T1) :-
	!,
	ffp(T-Body, T2-Body1),
	call(Body1),
	after_expansion(T2, T1).

after_expansion(X, X1) :-
	instantiate_prolog_vars_with_fresh_symbols(X),
	( varfun_mode ->
	  vfterm_to_term(X, X1)
	; X1 = X
	).

varfun_mode :-
	current_prolog_flag(allow_variable_name_as_functor, true).

instantiate_prolog_vars_with_fresh_symbols(X) :-
	term_variables(X, Vs),
	map_logform_gen_symbol(Vs).

map_logform_gen_symbol([X|Xs]) :-
	%% result can also serve as predicate symbol
	logform_gen_symbol(X),
	map_logform_gen_symbol(Xs).
map_logform_gen_symbol([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% TPTP EXPORT
%%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- flag(tptp_gen_id, _, 1).

mac_to_tptp(Label, TPTPStmts) :-
	flag(tptp_gen_id, _, 1),
	mac_expand_toplevel(Label, F),
	( ( F = (A -> C) ; F = (C <- A) ) ->
	  conj_to_list(A, Axioms),
	  map_mk_tptp_axiom(Axioms, Axioms1),
	  mk_tptp_stmt(C, conjecture, Conjecture),
	  append(Axioms1, [Conjecture], TPTPStmts)
	; mk_tptp_stmt(F, conjecture, Conjecture),
	  TPTPStmts = [Conjecture]
	).

map_mk_tptp_axiom([X|Xs], [X1|Xs1]) :-
	mk_tptp_stmt(X, axiom, X1),
	map_mk_tptp_axiom(Xs, Xs1).
map_mk_tptp_axiom([], []).

mk_tptp_stmt(F, Kind, fof(Label, Kind, F1)) :-
	( mac_has_def(F) ->
	  ( atom(F), \+ sub_atom(F, 0, _, _, 'axiom_') ->
	    Label = F
	  ; flag_inc(tptp_gen_id, N),
	    concat_atom(['axiom_', N], Label)
	  ),
	  mac_expand(F, F2),
	  nf_to_tptp_1(F2, Kind, F1)
	; flag_inc(tptp_gen_id, N),
	  concat_atom(['axiom_', N], Label),
	  nf_to_tptp_1(F, Kind, F1)
	).

nf_to_tptp_1(F, Kind, F1) :-
	( logform_has_second_order_quantifier(F) ->
	  ( Kind = axiom ->
	    elim_by_renaming_for_validity((F -> true), (F2 -> true))
	  ; Kind = conjecture ->
   	    elim_by_renaming_for_validity((false -> F), (false -> F2))
	  ),
	  ( logform_has_second_order_quantifier(F2) ->
	    err('Second-order quantifiers that can not be eliminated by renaming')
	  ; true
	  )
	; F2 = F
	),
	nf_to_tptp(F2, F1).
	

conj_to_list((X,Y), [X|Ys]) :-
	!,
	conj_to_list(Y, Ys).
conj_to_list(X, [X]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 
%%%% TPTP IMPORT
%%%% 
%%%%
%%%% tptp_to_mac('MSC/MSC006-1', X), ppl_content(X, [rightcols=3,maxpos=50],
%%%% '/Users/ch/tmp/oy2.tex')
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tptp_to_mac(ProblemSpec, X) :-
	tptp_read(ProblemSpec, Problem),
	tptp_label(ProblemSpec, Label),
	validate_tptp_problem(Problem),
	tptp_problem_format(Problem, Format),
	( Format = cnf ->
	  tptp_cnf_to_mac(Problem, Label, X)
	; Format = fof ->
	  tptp_fof_to_mac(Problem, Label, X)
	; err('Unsupported TPTP format for tptp_to_mac: ~q', [Format])
	).

tptp_label(ProblemSpec, Label) :-
	file_base_name(ProblemSpec, Label).
	
tptp_fof_to_mac(Problem, Label, [(def(Label) :: GoalForm)|Stmts]) :-
	map_tfm(Problem, Stmts, As, Cs),
	list_to_conjunction(As, As1),
	list_to_conjunction(Cs, Cs1),
	GoalForm = (Cs1 <- As1).

map_tfm([K-F|KFs], [(def(Label) :: F)|Stmts], As, Cs) :-
	( memberchk(name=Label, K), memberchk(role=Role, K) -> true
	; err('Insufficient information about formula in ~q', [K])
	),
	( memberchk(Role, [axiom, hypothesis, definition, lemma]) ->
	  As = [Label|As1],
	  Cs = Cs1
	; Role=conjecture ->
	  As = As1,
	  Cs = [Label|Cs1]
	; err('Unsupported fof formula role: ~q', [Role])
	),
	map_tfm(KFs, Stmts, As1, Cs1).
map_tfm([], [], [], []).

tptp_cnf_to_mac(Problem, Label, [(def(Label) :: GoalForm)|Stmts]) :-
	map_tcm(Problem, Stmts, As, Cs),
	list_to_conjunction(As, As1),
	list_to_conjunction(Cs, Cs1),
	negate(Cs1, Cs2),
	GoalForm = (Cs2 <- As1).

map_tcm([K-F|KFs], Stmts, As, Cs) :-
	( memberchk(name=Label, K), memberchk(role=Role, K) -> true
	; err('Insufficient information about formula in ~q', [K])
	),
	( memberchk(Role, [axiom, hypothesis, definition, lemma]) ->
	  As = [Label|As1],
	  Cs = Cs1,
	  clause_to_form(F, F1),
	  Stmts = [(def(Label) :: F1)|Stmts1]
	; Role=negated_conjecture ->
	  As = As1,
	  Stmts = Stmts1,
	  clause_to_form(F, F1),
	  Cs = [F1|Cs1]
	; err('Unsupported cnf formula role: ~q', [Role])
	),
	map_tcm(KFs, Stmts1, As1, Cs1).
map_tcm([], [], [], []).

clause_to_form(C, F) :-
	m_signature([C], _, Fs),
	( member(X/0, Fs),
	  atom_prefix(X, x),
	  sub_atom(X, _, 1, 0, Last),
	  char_type(Last, digit) ->
	  err('Clause contains reserved constant ~q', [X])
	; true
	),
	matrix_to_form_keep_skolems([C], F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% DEPRECATED: FROM OLD TOYELIM
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%% 
% %%%% Tools for Debugging KBs
% %%%% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% 
% %%%% 
% %%%% F is the full KB as a first-order sentence. Useful in connection
% %%%% with other predicates that operate on first-order sentences.
% %%%% 
% kb(Spec, F) :-
% 	findall(G, ((Label :: G), (memberchk(Label, Spec))), Gs),
% 	list_to_conjunction(Gs, F).
% 
% factbase(Spec, F) :-
% 	findall(G, ((Label :: G),
% 		    Label \= rules(_),
% 		   (memberchk(Label, Spec))), Gs),
% 	list_to_conjunction(Gs, F).
% 
% kb(F) :-
% 	kb([_], F).
% 
% factbase(F) :-
% 	factbase([_], F).
% 
% 
% %%%%
% %%%% Consider the whole KB as a first-order sentence and print out
% %%%% its predicates and functions. Useful to find typos.
% %%%% 
% print_signature :-
% 	kb(X),
% 	f_signature(X, Ps, Fs),
%         writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
% 	writeln('%%%% Predicates:'),
% 	writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
% 	( member(P, Ps),
% 	  writeq(P),
% 	  nl,
% 	  fail
% 	; true
% 	),
% 	writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
% 	writeln('%%%% Functions:'),
% 	writeln('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
% 	( member(F, Fs),
% 	  writeq(F),
% 	  nl,
% 	  fail
% 	; true
% 	).
% 
% 
% %%%% 
% %%%% Assert all syntactic ground facts of the KB as Prolog facts.
% %%%% Useful for debugging or playing around.
% %%%% 
% assert_factbase_in_prolog :-
% 	factbase(F),
% 	conjunction_to_list(F, Fs),
% 	( member(G, Fs),
% 	  cnf(G, G1),
% 	  ( G1 = [[G2]], ground(G2), \+ catch(G2, _, fail) ->
% 	    assert(G2)
% 	  ; true
% 	  ),
% 	  fail
% 	; true
% 	).
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match(X, Y) :-
	subsumes_term(X, Y),
	X = Y.
