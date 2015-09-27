:- module(rtchecks_tr, [rtchecks_sentence_tr/4,
			rtchecks_goal_tr/3],
	  [assertions, nortchecks, nativeprops, isomodes, dcg, hiord]).

:- if(current_prolog_flag(dialect, swi)).
% runtime-checks related flags:
:- use_module(rtchecks(rtchecks_flags), []).
:- use_module(library(lists)).
:- use_module(rtchecks(rtchecks_basic)).
:- endif.
:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(compiler(c_itf_internal)),
	    [(discontiguous)/3, defines_module/2, exports_pred/3, location/1,
	     location/3]).
:- use_module(library(sort)).
:- use_module(library(llists)).
:- use_module(library(aggregates)).
:- endif.
% see formulae, conj_to_list/2, list_to_conj/2
:- use_module(library(terms)).
:- use_module(rtchecks(rtchecks_gen)).
    
:- doc(author, "Edison Mera").

:- doc(module, "This module provides and implements the run-time
checking of predicate assertions transforming the procedure
definitions.

The semantic of run-time checks is explained in the paper:

@href{http://clip.dia.fi.upm.es/papers/assert-lang-disciplbook_bitmap.pdf}

The rtchecks are generated in such a way that redundant tests are
collapsed, in order to avoid overhead.

Several instrumentation modes are provided to implement the run-time
checks. One is inline, which instrument the code directly. The other
mode is library, that use external metapredicates to do the checks,
this mode save space but could have less performance.

Note that the instrumentation can be done in the call to predicates
(transforming calls), or in the procedure definitions (transforming
procedure definitions), which is the instrumentation method
implemented here.

IMPORTANT NOTE: This package has been made compatible with SWI-Prolog
at source/level, via an improved ciao dialect support in SWI, the
usage of a customized assertion reader library (assrt_lib.pl) and
some conditional compiler declarations.  Any change to this code must
be done in such a way that the compability is not compromised.

").

:- doc(bug, "Currently we can not deal with dynamic/data
	predicates. --EMM").

:- doc(bug, "The unit-tests fails when I use in a test a regular
	type defined in the same module that is not exported. To solve
	it the runtime-check package must export the regular type
	automatically, and perhaps, to show a warning informing the
	user about such situation. --EMM").

:- doc(bug, "Assertions must be defined before the predicate. This
	is deliberate, since we preferred not to have to process all
	the predicates, clauses and assertions at the end, losing the
	locator information and confusing the debugger. In case of an
	assertion defined later, a warning message is shown,
	recommending to write a discontiguous directive (?). --EMM").

:- doc(bug, "Unimplemented: another level of instrumentation
	should be defined to report what literal caused the throw of a
	runtime check error, when such literal is not using the
	rtchecks package. This should be done implementing the
	``transforming calls'' instrumentation method.  --EMM").

:- doc(bug, "Runtime-checks for builtins or implementation-defined
	predicates not implemented yet.  Idea: we can use the
	redefining/1 declaration in order to work around this
	problem. --EMM").

:- doc(bug, "Unify it with the rtchecks package by David Trallero.
	I am not using rtchecks by DTM because:
	 @item it depends on ciaopp
	 @item to use it you should use transform
	 @item it does not do a clean inline instrumentation of the code.
	 @item I tried to use a full set of assertions but it does not
	       generate the code correctly w.r.t. the specification.
--EMM").

:- doc(bug, "Redundant tests in [entry, exit] assertions are not
	collapsed with [calls, success] assertions. It is a bug or a
	behavior???, I am not sure, because [calls, success] are more
	restrictive and is not necessary to put it together with
	[entry, exit] assertions. --EMM").

:- doc(bug, "@var{Compat} are compatibility properties, while
	@var{Call} and @var{Succ} are instantiation properties.  Note
	that the semantic should be the same as in CiaoPP, but I see
	that in CiaoPP compat properties are currently ignored
	(?). --EMM").

:- if(current_prolog_flag(dialect, swi)).
:- multifile
    generated_rtchecks_db/3,
    head_alias_db/3,
    goal_alias_db/3,
    posponed_sentence_db/8.

% BUG: In SWI modules are extensible, and therefore, we should not clean up
% rtchecks information at the end (multifiles, consult/1 in the same context,
% etc...) however, this is not working yet --EMM
%

:- endif.

:- data
    generated_rtchecks_db/3,
    head_alias_db/3,
    goal_alias_db/3,
    posponed_sentence_db/8.

cleanup_db(M) :-
	retractall_fact(goal_alias_db(_, _, M)),
	cleanup_db_0(M).

cleanup_db_0(M) :-
	retractall_fact(head_alias_db(_, _, M)),
	retractall_fact(posponed_sentence_db(_, _, _, _, _, _, M, _)),
	retractall_fact(generated_rtchecks_db(_, _, M)).

:- if(current_prolog_flag(dialect, swi)).

:- multifile nortchecked/1.

ciao:declaration_hook(nortchecked, rtchecks_tr:nortchecked(M)) :-
    '$set_source_module'(M, M).

:- dynamic location/3.
location(Loc) :-
	ignore((( location(Src, Ln0, Ln1)
		; source_location(Src, Ln0 ),
		  Ln0 = Ln1
		),
		Loc = loc(Src, Ln0, Ln1)
	       )).

:- endif.

proc_posponed_sentence(Clauses, M) :-
	posponed_sentence_db(F, A, Head, Op, Body, loc(S, LB, LE), M, Dict),
	asserta_fact(location(S, LB, LE), Ref),
	transform_sentence(F, A, Head, Op, Body, Clauses, M, Dict),
	erase(Ref).

remaining_pred(F, A, M) :-
	current_assertion(_, rtcheck, _, _, _, _, _, _, _, _, _, _, _, F, A, M),
	\+ generated_rtchecks_db(F, A, M).

remaining_preds(Preds, M) :-
	findall(F/A, remaining_pred(F, A, M), Preds0),
	sort(Preds0, Preds).

:- if(current_prolog_flag(dialect, swi)).
add_redefine_declaration(_, _, H) -->
	( { predicate_property(H, imported_from(L)),
	    module_property(L, class(C)),
	    memberchk(C, [system, library])
	  }
	->[(:- redefine_system_predicate(H))]
	; []
	).

add_meta_declaration(M, H) -->
	( {predicate_property(M:H, meta_predicate(Spec))} ->
	  [(:- meta_predicate(Spec))]
	; []
	).
:- endif.

:- if(current_prolog_flag(dialect, ciao)).
add_redefine_declaration(F, A, _) -->
	[(:- redefining(F/A))].

add_meta_declaration(_, _) --> []. % TODO: Test in Ciao
:- endif.

add_export_declaration(F, A) -->
	[(:- export(F/A))].	% Give the opportunity to reuse the
	                   	% run-time checked version of the
	                   	% imported predicate.

add_declarations(F, A, M, H) -->
	{functor(H, F, A)},
	add_redefine_declaration(F, A, H),
	add_meta_declaration(M, H),
	add_export_declaration(F, A).

proc_remaining_assertions(Preds, Clauses, M, Dict) :-
	member(F/A, Preds),
	add_declarations(F, A, M, Head, Clauses, Clauses1),
	transform_sentence_body(Dict, Head, (:-), F, A, M, original,
				'$orig_call'(Head), Clauses1).

:- if(current_prolog_flag(dialect, swi)).
runtime_checkable(M) :-
	\+ nortchecked(M),
	current_prolog_flag(runtime_checks, yes).
:- else.
runtime_checkable(_) :-
	current_prolog_flag(runtime_checks, yes).
:- endif.

proper_warning_flags(C, C, T) :- C == T, !, T = [end_of_file].
:- if(current_prolog_flag(dialect, swi)).
proper_warning_flags(C,
		     [(:- style_check(-discontiguous))|C],
		     [(:- style_check(+discontiguous)), end_of_file]).
:- endif.
:- if(current_prolog_flag(dialect, ciao)).
proper_warning_flags(C,
		     [(:- push_prolog_flag(discontiguous_warnings, off)),
		      (:- push_prolog_flag(multi_arity_warnings,   off))|C],
		     [(:- pop_prolog_flag(discontiguous_warnings)),
		      (:- pop_prolog_flag(multi_arity_warnings)),
		      end_of_file]).
:- endif.

rtchecks_sentence_tr(0, _, M, _) :- !, cleanup_db(M).
rtchecks_sentence_tr(end_of_file, Clauses, M, _) :- !,
	runtime_checkable(M),
	module_property(M, file(File)),
	prolog_load_context(file, File), % This is the main file
	findall(Clauses0, proc_posponed_sentence(Clauses0, M), ClausesL0,
	    ClausesL1),
	remaining_preds(Preds, M),
	findall(Clauses1, proc_remaining_assertions(Preds, Clauses1, M, []),
	    ClausesL1, Tail),
	proper_warning_flags(ClausesL0, ClausesL2, Tail),
	flatten(ClausesL2, Clauses),
	cleanup_db_0(M).

rtchecks_sentence_tr(Sentence0, Sentence, M, Dict) :-
	do_rtchecks_sentence_tr(Sentence0, Sentence, M, Dict).

do_rtchecks_sentence_tr((Head --> Body), Clauses, M, Dict) :-
	!,
	process_sentence(Head, Body, (-->), Clauses, M, Dict).
do_rtchecks_sentence_tr((Head :- Body), Clauses, M, Dict) :-
	!,
	process_sentence(Head, Body, (:-), Clauses, M, Dict).
do_rtchecks_sentence_tr((:- _Decl), _, _, _) :-
	!,
	fail.
do_rtchecks_sentence_tr(Head, Clauses, M, Dict) :-
	process_sentence(Head, true, (:-), Clauses, M, Dict).

rtchecks_goal_tr(end_of_file, _,         M) :- !, cleanup_db(M).
rtchecks_goal_tr(PPAssertion, PPRTCheck, _) :-
	proc_ppassertion(PPAssertion, Loc, PPRTCheck),
	location(Loc), !.
rtchecks_goal_tr('$orig_call'(Goal0), Goal, M) :- !,
	qualify_goal(M, Goal0, Goal). % TODO: doesn't work with builtins (swi) --EMM
:- if(current_prolog_flag(dialect, ciao)).
rtchecks_goal_tr('$meta$rtc'(Goal, MG), MG=RM:Goal, M) :- !,
	module_qualifier_i(M, Goal, RM).
:- endif.
rtchecks_goal_tr(Goal, Goal1, M) :-
	goal_alias_db(Goal, Goal1, M), !.

:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(compiler(c_itf_internal)),
	[(multifile)/3, defines/3, imports/5]).
imported_from(M:Goal, EM) :-
	functor(Goal, F, N),
	imports(M, _IM, F, N, EM).

:- comment(bug, "Currently this will have problems with multifile predicates. --EMM").
module_qualifier_i(M, Goal, RM) :-
	functor(Goal, F, N),
	( multifile(M, F, N) -> RM = M % multifile
	; imports(M, _IM, F, N, EM) -> RM = EM % Imported have priority
	; defines(M, F, N) -> RM = M
	; RM = M
	).

is_discontiguous(M:Head) :-
	functor(Head, F, A),
	discontiguous(F, A, Base),
	defines_module(Base, M).

needs_posponed_definition(Goal) :- is_discontiguous(Goal).

:- endif.

:- if(current_prolog_flag(dialect, swi)).

:- meta_predicate imported_from(+,-). % Stop automatic module qualification

imported_from(Goal, EM) :- predicate_property(Goal, imported_from(EM)).

/*
module_qualifier_i(M, Goal, RM) :-
    ( imported_from(M:Goal, EM) -> RM = EM % Imported have priority
    ; '$set_source_module'(RM, RM) -> true
    ; RM = M
    ).
*/

% is_discontiguous(Pred) :- '$get_predicate_attribute'(Pred, (discontiguous), 1).

needs_posponed_definition(Goal) :- false. % is_discontiguous(Goal).

:- endif.

qualify_goal(M, Goal0, Goal) :-
	( imported_from(M:Goal0, EM) ->
	  Goal = EM:Goal0
	;
	  % functor(Goal0, _, N),
	  % rename_head('1', N, Goal0, Goal)
	  atom_concat(M, '$impl', IM),
	  Goal = IM:Goal0 % Last opportunity: define Goal0 in IM
	).

extend_head((:-),  Head,  Head) :- !.
extend_head((-->), Head0, Head) :-
	Head0 =.. ArgL0,
	append(ArgL0, [_, _], ArgL),
	Head =.. ArgL.

process_sentence(Head0, Body, Op, Clauses, M, Dict) :-
	extend_head(Op, Head0, Head),
	functor(Head, F, A),
	( needs_posponed_definition(M:Head) ->
	  location(Loc),
	  assertz_ct(posponed_sentence_db(F, A, Head, Op, Body, Loc, M, Dict)),
	  Clauses = []
	; transform_sentence(F, A, Head, Op, Body, Clauses, M, Dict)
	).

reduce_head(Head0, Head) :-
	Head0 =.. ArgL0,
	append(ArgL, [_, _], ArgL0),
	Head =.. ArgL.

head_body_clause((:-), Head, Body, Clause) :- !,
	( Body == true ->
	  Clause = Head
	; Clause = (Head :- Body)
	).
head_body_clause((-->), Head0, Body, (Head --> Body)) :-
	reduce_head(Head0, Head).

:- if(current_prolog_flag(dialect, swi)).
:- meta_predicate assertz_ct(:).
assertz_ct(Fact) :- compile_aux_clauses(Fact).
:- endif.
:- if(current_prolog_flag(dialect, ciao)).
assertz_ct(Fact) :- assertz_fact(Fact).
:- endif.

mark_generated_rtchecks(F, A, M) :-
	assertz_ct(generated_rtchecks_db(F, A, M)).

transform_sentence_body(Dict, Head, Op, F, A, M, Flag, Body, Clauses) :-
	( generated_rtchecks_db(F, A, M) ->
	  head_alias_db(Head, Head1, M),
	  head_body_clause(Op, Head1, Body, Clause),
	  Clauses = [Clause]
	;
	  location(PLoc),
	  functor(Pred, F, A),
	  collect_assertions(Pred, M, rtcheck, Assertions),
	  ( Assertions \= [] ->
	    generate_rtchecks(F, A, M, Assertions, Pred, Dict, PLoc,
			      _, Head, Op, Body, Flag, Clauses0, [])
	  ->mark_generated_rtchecks(F, A, M),
	    Clauses0 = Clauses
	  ; head_body_clause(Op, Head, Body, Clause),
	    Clauses = [Clause]
	  )
	),
	!.
% transform_sentence_body(_, Head, _, _, _, Body, [(Head :- Body)]).

transform_sentence(F, A, Head, Op, Body, Clauses, M, Dict) :-
	current_prolog_flag(runtime_checks, yes),
	transform_sentence_body(Dict, Head, Op, F, A, M, transform, Body, Clauses).
transform_sentence(_, _, Head, Op, Body, Clause, _, _) :-
	head_body_clause(Op, Head, Body, Clause).

rename_head(Tag, A, Head, Head1) :-
	Head =.. [F|Args],
	atom_number(NA, A),
	atom_concat([F, '/', NA, '$rtc', Tag], F1),
	Head1 =.. [F1|Args].

record_head_alias(Head0, Head, M) :-
	functor(Head0, F0, A),
	functor(Pred0, F0, A),
	Pred0 =.. [_|Args],
	functor(Head, F, _),
	Pred =.. [F|Args],
	assertz_ct(head_alias_db(Pred0, Pred, M)).

record_goal_alias(Head0, Head, M) :-
	functor(Head0, F0, A),
	functor(Pred0, F0, A),
	Pred0 =.. [_|Args],
	functor(Head, F, _),
	Pred =.. [F|Args],
	assertz_ct(goal_alias_db(Pred0, Pred, M)).

generate_rtchecks(F, A, M, Assrs, Pred, PDict, PLoc, Pred2,
		  Head, Op, Body, Flag) -->
	{generate_step_rtchecks(step1, Assrs, Pred, M, PLoc, Body00, Body01)},
	( {Body00 \== Body01} ->
	  { rename_head('1', A, Pred, Pred1),
	    record_goal_alias(Pred, Pred1, M),
	    Body01 = Pred1,
	    lists_to_lits(Body00, Lits0 )
	  },
	  [(Pred :- Lits0 )]
	; {Pred = Pred1}
	),
	{generate_step_rtchecks(step2, Assrs, Pred, M, PLoc, Body1, Body12)},
	( {Body1 \== Body12} ->
	  ( {Flag = transform} ->
	    { rename_head('2', A, Pred, Pred2),
	      Body12 = Pred2,
	      functor(Pred1, F1, A1)
	    }
	  ; { Head = Pred,
	      Body12 = Body
	    }
	  ),
	  {lists_to_lits(Body1, Lits1)},
	  [(Pred1 :- Lits1)]
	; {Pred1 = Pred2}
	),
	( {Flag = transform} ->
	  { record_head_alias(Pred, Pred2, M), % TODO: Optimize this literal
	    head_alias_db(Head, Head1, M),
	    head_body_clause(Op, Head1, Body, Clause)
	  },
	  [Clause]
	; []
	).
