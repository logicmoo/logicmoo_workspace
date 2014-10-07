:- module(rtchecks_tr, [rtchecks_sentence_tr/4, valid_commands/1,
			rtchecks_goal_tr/3, collect_assertions/4,
			generate_rtchecks/8],
	  [assertions, nortchecks, nativeprops, isomodes, dcg, hiord]).

:- multifile have_inline/0.

:- if(current_prolog_flag(dialect, swi)).
:- use_module(library(swi/nativeprops)).
% runtime-checks related flags:
:- include(rtchecks(rtchecks_flags)).
:- ( define_flag(Flag, _Values, Default),
     create_prolog_flag(Flag, Default, [type(atom)]),
     fail
   ; true
   ).
:- use_module(library(filesex)).
:- use_module(engine(basic_props)).
% For simplicity, for SWI-Prolog we only support tr_library:
:- use_package(rtchecks(rtchecks_tr_library)).
:- else.
% You can test the inliner package with this module
:- use_package(rtchecks(rtchecks_tr_inline)).
:- endif.
:- use_module(library(assertions(assrt_lib)),
	      [assertion_read/9, assertion_body/7,
	       comps_to_goal/3, comps_to_goal/4]).
:- if(current_prolog_flag(dialect, ciao)).
:- use_module(library(compiler(c_itf_internal)),
	    [(discontiguous)/3, defines_module/2, exports_pred/3, location/1,
	     location/3]).
:- use_module(library(inliner(inliner_tr)), [in_inline_module_db/2, inline_db/4,
					     lit_clause_arity/4]).
:- endif.
% see formulae, conj_to_list/2, list_to_conj/2
:- use_module(library(llists)).
:- use_module(library(aggregates)).
:- use_module(library(terms)).
:- use_module(library(sort)).
:- use_module(rtchecks(term_list)).

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
be done in such a way that the compability do not be compromised.

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

:- data head_alias_db/3.
:- data goal_alias_db/3.
:- if(current_prolog_flag(dialect, swi)).
:- multifile generated_rtchecks_db/3.
:- endif.
:- data generated_rtchecks_db/3.
% :- data rtchecks_db/3.
% :- data nortchecks_db/3.
:- data posponed_sentence_db/7.

valid_assertions(Status, Type) :-
	rtcheck_assr_type(Type),
	( Type = (entry)
	->Status = true
	; rtcheck_assr_status(Status)
	).

% rtcheck_assr_status(true) :- current_prolog_flag(rtchecks_true, yes).
rtcheck_assr_status(trust) :- current_prolog_flag(rtchecks_trust, yes).
rtcheck_assr_status(debug) :- current_prolog_flag(rtchecks_debug, yes).
rtcheck_assr_status(trace) :- current_prolog_flag(rtchecks_trace, yes).
rtcheck_assr_status(check) :- current_prolog_flag(rtchecks_check, yes).

rtcheck_assr_type(calls).
rtcheck_assr_type(entry) :- current_prolog_flag(rtchecks_entry, yes).
rtcheck_assr_type(pred).
rtcheck_assr_type(prop).
rtcheck_assr_type(test) :- current_prolog_flag(rtchecks_test, yes).
rtcheck_assr_type(comp).
rtcheck_assr_type(exit) :- current_prolog_flag(rtchecks_exit, yes).
rtcheck_assr_type(success).

:- if(have_inline).

add_cond_decl(Body0, Body, Decl, Clauses0, Clauses) :-
	( Body0 == Body -> Clauses = Clauses0
	; Clauses = [(:- Decl)|Clauses0]
	).

add_cond_use_inline(Body0, Body, F, A, Clauses0, Clauses) :-
	add_cond_decl(Body0, Body, use_inline(F/A), Clauses0, Clauses).

add_use_inline(F, A, Clauses, [(:- use_inline(F/A))|Clauses]).

insert_inline_declarations(F, A, M, Head, Clauses,
	                   [(:- inline(F/A)), (:- inline(F1/A1))|Clauses]) :-
	head_alias_db(Head, Head1, M),
	functor(Head1, F1, A1).

:- else.

add_cond_use_inline(_, _, _, _, Clauses, Clauses).

add_use_inline(_, _, Clauses, Clauses).

insert_inline_declarations(_, _, _, _, Clauses, Clauses).

inline_db(_, _, _, _) :- fail.

:- endif.

:- if(current_prolog_flag(dialect, swi)).

:- multifile nortchecked/1.

:- multifile ciao:declaration_hook/2.
ciao:declaration_hook(nortchecked, rtchecks_tr:nortchecked(M)) :-
    '$set_source_module'(M, M).

rel_file_name(ASrc, Src) :-
    ( working_directory(Dir, Dir),
      directory_file_path(Dir, Src, ASrc)
    ->true
    ; Src = ASrc
    ).

:- dynamic location/3.
location(Loc) :-
	ignore((( location(ASrc, Ln0, Ln1)
		; source_location(ASrc, Ln0 ),
		  Ln0 = Ln1
		),
		rel_file_name(ASrc, Src),
		Loc = loc(Src, Ln0, Ln1)
	       )).

lit_clause_arity(_, _, A, A).

:- meta_predicate exported_predicate(spec).
exported_predicate(M:F/A) :-
	functor(H, F, A),
	predicate_property(M:H, export).
:- endif.

:- if(current_prolog_flag(dialect, ciao)).
exported_predicate(M:F/A) :- 
	defines_module(Base, M),
	( exports_pred(Base, all, all)
	; exports_pred(Base, F, A)
	).
:- endif.

proc_posponed_sentence(Clauses0, M) :-
	posponed_sentence_db(F, A, Head, Body0, loc(S, LB, LE), M, Dict),
	asserta_fact(location(S, LB, LE), Ref),
	transform_sentence(F, A, Head, Body0, Body, Clauses, M, Dict),
	add_cond_use_inline(Body0, Body, F, A, Clauses0, Clauses),
	erase(Ref).

remaining_preds(Preds, M) :-
	findall(F/A, remaining_pred(F, A, M), Preds0),
	sort(Preds0, Preds).

/*
  Combination of status and rtcheck indicators, to control the compile
  and run-time checking:

  ===========+============++===========+==========
  Status     | + Command  || ctchecked | rtchecked
  ===========+============++===========+==========
  true/trust | -          || no        | no
  true/trust | rtcheck    || no        | yes
  check      | no_rtcheck || yes       | no
  check      | -          || yes       | yes
  ===========+============++===========+==========
  Note: Is weird to preserve ciao-compatibility
*/

assertion_is_valid(ctcheck, Status, Type, _) :-
    valid_ctcheck_assertions(Status, Type).
assertion_is_valid(rtcheck, Status, Type, Comp) :-
	( \+ memberchk(_:rtcheck(_), Comp) ->
	  valid_assertions(Status, Type),
	  \+ memberchk(_:no_rtcheck(_), Comp)
	; true % Force run-time checking
	).

valid_ctcheck_assertions(Status, Type) :-
    ctcheck_assr_status(Status),
    ctcheck_assr_type(Type).

ctcheck_assr_status(trust).
ctcheck_assr_status(check).

ctcheck_assr_type(calls).
ctcheck_assr_type(entry).
ctcheck_assr_type(pred).
ctcheck_assr_type(prop).
ctcheck_assr_type(exit).
ctcheck_assr_type(success).

black_list_pred('=', 2).

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
	Body = '$orig_call'(Head),
	current_prolog_flag(rtchecks_predloc, UsePredLoc),
	setup_call_cleanup(
	    set_prolog_flag(rtchecks_predloc, no),
	    transform_sentence_body(Dict, Head, F, A, M, Body, Body, Clauses0),
	    set_prolog_flag(rtchecks_predloc, UsePredLoc)),
	insert_inline_declarations(F, A, M, Head, Clauses0, Clauses1).

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
		     
rtchecks_sentence_tr(0, _, _, _) :- !.
:- endif.
rtchecks_sentence_tr(end_of_file, Clauses, M, _) :-
	!,
	runtime_checkable(M),
	findall(Clauses0, proc_posponed_sentence(Clauses0, M), ClausesL0,
	    ClausesL1),
	remaining_preds(Preds, M),
	findall(Clauses1, proc_remaining_assertions(Preds, Clauses1, M, []),
	    ClausesL1, Tail),
	proper_warning_flags(ClausesL0, ClausesL2, Tail),
	flatten(ClausesL2, Clauses),
	cleanup_db_0(M).
:- if(current_prolog_flag(dialect, ciao)).
rtchecks_sentence_tr(_, _, M, _) :-
	in_inline_module_db(_, M),
	!,
	fail.
:- endif.
rtchecks_sentence_tr(Sentence0, Sentence, M, Dict) :-
	do_rtchecks_sentence_tr(Sentence0, Sentence, M, Dict).

do_rtchecks_sentence_tr((_ --> _), _, _, _) :- !, fail. % TODO: dcg support here
do_rtchecks_sentence_tr((Head :- Body), Clauses, M, Dict) :-
	!,
	process_sentence(Head, Body, Clauses, M, Dict).
do_rtchecks_sentence_tr((:- _Decl), _, _, _) :-
	!,
	fail.
do_rtchecks_sentence_tr(Head, Clauses, M, Dict) :-
	process_sentence(Head, true, Clauses, M, Dict).

proc_ppassertion(check(Goal), PredName, Dict, Loc,
		 rtcheck(Goal, PredName, Dict, Loc)).
proc_ppassertion(trust(Goal), PredName, Dict, Loc, RTCheck) :-
	( current_prolog_flag(rtchecks_trust, yes) ->
	  RTCheck = rtcheck(Goal, PredName, Dict, Loc)
	; RTCheck = true
	).
proc_ppassertion(true(_),  _, _, _, true).
proc_ppassertion(false(_), _, _, _, true).

rtchecks_goal_tr(end_of_file, _,         M) :- !, cleanup_db(M).
rtchecks_goal_tr(PPAssertion, PPRTCheck, _) :-
	proc_ppassertion(PPAssertion, PredName, [], Loc, PPRTCheck),
	location(Loc),
	PredName = PPAssertion,
	!.
rtchecks_goal_tr('$orig_call'(Goal0), Goal, M) :-
	qualify_goal(M, Goal0, Goal). % TODO: doesn't work with builtins (swi) --EMM
:- if(current_prolog_flag(dialect, ciao)).
rtchecks_goal_tr('$meta$rtc'(Goal, MG), MG=RM:Goal, M) :-
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

needs_posponed_definition(Goal) :- is_discontiguous(Goal).

is_discontiguous(M:Head) :-
	functor(Head, F, A),
	discontiguous(F, A, Base),
	defines_module(Base, M).
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

needs_posponed_definition(_) :- fail. % is_discontiguous(Goal).

% is_discontiguous(Pred) :- '$get_predicate_attribute'(Pred, (discontiguous), 1).
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

cleanup_db_0(M) :-
	cleanup_head_alias_db(M),
	retractall_fact(posponed_sentence_db(_, _, _, _, _, M, _)),
	cleanup_generated_rtchecks_db(M).

cleanup_db(M) :-
	cleanup_goal_alias_db(M),
	cleanup_db_0(M).

cleanup_head_alias_db(M) :-
	retractall_fact(head_alias_db(_, _, M)).

cleanup_goal_alias_db(M) :-
	retractall_fact(goal_alias_db(_, _, M)).

:- if(current_prolog_flag(dialect, swi)).
cleanup_generated_rtchecks_db(_).
:- else.
cleanup_generated_rtchecks_db(M) :-
	retractall_fact(generated_rtchecks_db(_, _, M)).
:- endif.

process_sentence(Head, Body0, Clauses, M, Dict) :-
	functor(Head, F, A),
	( needs_posponed_definition(M:Head) ->
	  location(Loc),
	  assertz_fact(posponed_sentence_db(F, A, Head, Body0, Loc, M, Dict)),
	  Clauses = []
	; transform_sentence(F, A, Head, Body0, Body, Clauses0, M, Dict),
	  add_cond_use_inline(Body0, Body, F, A, Clauses0, Clauses)
	).

head_body_clause(Head, Body, Clause) :-
	( Body == true ->
	  Clause = Head
	; Clause = (Head :- Body)
	).

:- if(current_prolog_flag(dialect, swi)).
mark_generated_rtchecks(F, A, M, C, [rtchecks_tr:generated_rtchecks_db(F, A, M)|C]).
:- endif.
:- if(current_prolog_flag(dialect, ciao)).
mark_generated_rtchecks(F, A, M, C, C) :-
    assertz_fact(generated_rtchecks_db(F, A, M)).
:- endif.

transform_sentence_body(Dict, Head, F, A, M, Body0, Body, Clauses) :-
	( generated_rtchecks_db(F, A, M) ->
	  head_alias_db(Head, Head1, M),
	  Clauses = (Head1 :- Body)
	;
	  location(PLoc),
	  functor(Pred, F, A),
	  collect_assertions(Pred, M, rtcheck, Assertions),
	  ( Assertions \= [] ->
	    current_prolog_flag(rtchecks_asrloc,  UseAsrLoc),
	    current_prolog_flag(rtchecks_predloc, UsePredLoc),
	    generate_rtchecks(F, A, M, Assertions, Pred, Dict, PLoc,
			      (UsePredLoc, UseAsrLoc), _, Head, Body0, Body, Clauses0, []) ->
	    mark_generated_rtchecks(F, A, M, Clauses0, Clauses)
	  ; head_body_clause(Head, Body, Clause),
	    Clauses = [Clause]
	  )
	),
	!.
% transform_sentence_body(_, Head, _, _, _, Body, [(Head :- Body)]).

transform_sentence(F, A, Head, Body0, Body, Clauses, M, Dict) :-
	current_prolog_flag(runtime_checks, yes),
	process_body(Dict, Head, F, A, M, Body0, Body),
	transform_sentence_body(Dict, Head, F, A, M, Body0, Body, Clauses).
transform_sentence(_, _, Head, Body, Body, Clause, _, _) :-
	head_body_clause(Head, Body, Clause).

:- export(body_expansion/3).
:- meta_predicate body_expansion(?, pred(2), ?).

body_expansion(Goal0, P, Goal) :-
	var(Goal0),
	!,
	call(P, Goal0, Goal).
body_expansion((A, B), P, (NA, NB)) :-
	!,
	body_expansion(A, P, NA),
	body_expansion(B, P, NB).
body_expansion((A; B), P, (NA; NB)) :-
	!,
	body_expansion(A, P, NA),
	body_expansion(B, P, NB).
body_expansion((A->B), P, (NA->NB)) :-
	!,
	body_expansion(A, P, NA),
	body_expansion(B, P, NB).
body_expansion((X^A), P, (X^NA)) :-
	!,
	body_expansion(A, P, NA).
body_expansion((\+ A), P, (\+ NA)) :-
	!,
	body_expansion(A, P, NA).
body_expansion(if(A, B, C), P, if(NA, NB, NC)) :-
	!,
	body_expansion(A, P, NA),
	body_expansion(B, P, NB),
	body_expansion(C, P, NC).
body_expansion(Goal0, P, Goal) :-
	call(P, Goal0, Goal),
	!.
body_expansion(Goal, _, Goal).

ppassr_expansion(Goal, _, _, _, Goal) :-
	var(Goal),
	!.
ppassr_expansion(PPAssertion, PredName, Dict, Loc, Goal) :-
	proc_ppassertion(PPAssertion, PredName, Dict, Loc, Goal).

process_body(Dict, Pred, F, A, M, Body0, Body) :-
	( current_prolog_flag(rtchecks_callloc, literal) ->
	  body_expansion(Body0, calllit_expansion(Dict, PredName0, Loc0), Body1)
	; body_expansion(Body0, ppassr_expansion(PredName0, Dict, Loc0), Body1)
	),
	(
	    Body0 == Body1 ->
	    Body = Body1
	;
	    location(Loc),
	    current_prolog_flag(rtchecks_namefmt, NameFmt),
	    get_predname(NameFmt, Dict, Pred, PredName),
	    collapse_terms(Body1, [Loc = Loc0, PredName = PredName0], Terms),
	    lists_to_lits([Terms, Body1], Body),
	    functor(Pred, F, A)
	).

put_call_stack(Goal0, Pos, Goal) :- Goal = call_stack(Goal0, Pos).

calllit_expansion(Goal0, PDict, PredName, Loc, Goal) :-
	var(Goal0),
	!,
	current_prolog_flag(rtchecks_namefmt, NameFmt),
	get_predname(NameFmt, PDict, Goal0, LitName),
	put_call_stack(Goal0, litloc(LitName, Loc-PredName), Goal).
calllit_expansion(!,           _,     _,        _,   !) :- !.
calllit_expansion(true,        _,     _,        _,   true) :- !.
calllit_expansion(A =.. B,     _,     _,        _,   A =.. B) :- !.
calllit_expansion(call(Goal0), _,     _,        _,   call(Goal0)) :- !.
calllit_expansion(PPAssertion, PDict, PredName, Loc, PPRTCheck) :-
	proc_ppassertion(PPAssertion, PredName, PDict, Loc, PPRTCheck),
	!.
calllit_expansion(Goal0, PDict, PredName, Loc, Goal) :-
	current_prolog_flag(rtchecks_namefmt, NameFmt),
	get_predname(NameFmt, PDict, Goal0, LitName),
	put_call_stack(Goal0, litloc(LitName, Loc-PredName), Goal).

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
	assertz_fact(head_alias_db(Pred0, Pred, M)).

record_goal_alias(Head0, Head, M) :-
	functor(Head0, F0, A),
	functor(Pred0, F0, A),
	Pred0 =.. [_|Args],
	functor(Head, F, _),
	Pred =.. [F|Args],
	assertz_fact(goal_alias_db(Pred0, Pred, M)).


/*

Algorithm:

pred :- body.

is transformed in:

pred :-                        \
	"check entry...",       \___________ STEP
	"check exit...",        /            ONE
	'pred$rtc1'.           /

'pred$rtc1' :-                            \
	"check compat pre..."              \
	"check calls...",                   \
	"check success pre",                 \__________ STEP
	"check comp..."(                     /           TWO
	call_stack('pred$rtc2', Loc)),      /
	"check success pos",               /
	"check compat pos..."             /

call_stack(Goal, Loc) :-
	intercept(Goal,
	    rtcheck(LocStack, ...),
	    send_signal(rtcheck([Loc|LockStack], ...))).

'pred$rtc2' :-
	body.

And goals preds are renamed to 'pred$rtc1'.  There are other steps in
order to simplify the generated code as far as possible.

*/

generate_common_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, CallAssrt, SuccAssrt, CompAssrt) -->
	compat_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, [], ChkCompatL0),
	calls_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    CallAssrt, [], CheckedL0),
	success_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    SuccAssrt, CheckedL0, CheckedL1),
	compatpos_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, ChkCompatL0),
	comp_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc, PosLocs,
	    CompAssrt, CheckedL1).

generate_step1_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc, Goal0, Goal) :-
	do_generate_step1_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc,
	    PosLocs0, Goal1, Goal),
	(reverse(PosLocs0, PosLocs1) -> true),
	collapse_terms(Goal1, PosLocs1, PosLocs2),
	reverse(PosLocs2, PosLocs),
	append(PosLocs, Goal1, Goal0).

do_generate_step1_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc, PosLocs) -->
	{
	    current_prolog_flag(rtchecks_level, Level0),
	    neg_level(Level0, Level),
	    compat_assrt(Level, [test], CompatAssrt, []),
	    call_assrt(Level, [entry], CallAssrt, []),
	    succ_assrt(Level, [exit, success, pred], SuccAssrt, []),
	    comp_assrt(Level, [comp, pred], CompAssrt, [])
	},
	generate_common_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, CallAssrt, SuccAssrt, CompAssrt).

generate_step2_rtchecks(Assertions, Pred, M, PDict, PLoc, UsePosLoc, Goal0,
	    Goal) :-
	do_generate_step2_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc, PosLocs0,
	    Goal1, Goal2),
	(
	    Goal1 == Goal2 ->
	    Goal = Goal2
	;
	    current_prolog_flag(rtchecks_namefmt, NameFmt),
	    current_prolog_flag(rtchecks_callloc, CallLoc),
	    generate_callloc(CallLoc, UsePosLoc, PDict, PLoc, PosLocs0, Pred,
		NameFmt, Goal2, Goal)
	),
	once(reverse(PosLocs0, PosLocs1)),
	collapse_terms(Goal1, PosLocs1, PosLocs2),
	reverse(PosLocs2, PosLocs),
	append(PosLocs, Goal1, Goal0).

do_generate_step2_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc, PosLocs) -->
	{
	    current_prolog_flag(rtchecks_level, Level),
	    compat_assrt(Level, [], CompatAssrt, []),
	    call_assrt(Level, [], CallAssrt, []),
	    succ_assrt(Level, [test, success, pred], SuccAssrt, []),
	    comp_assrt(Level, [test, comp, pred], CompAssrt, [])
	},
	generate_common_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc, PosLocs,
	    CompatAssrt, CallAssrt, SuccAssrt, CompAssrt).

% ----------------------------------------------------------------------------
neg_level(inner,   exports).
neg_level(exports, inner).

compat_assrt(exports, _) --> [].
compat_assrt(inner,  AL) -->
	findall((A,pred), (valid_assertions(A,pred), \+ memberchk(A, AL))).

call_assrt(Level, BL0) -->
	{Level = exports -> BL = BL0 ; BL= [calls, pred|BL0]},
	findall((A, B), (valid_assertions(A,B), memberchk(B, BL))).

succ_assrt(Level, BL0) -->
	{Level = exports -> subtract(BL0, [test, success, pred], BL);BL = BL0},
	findall((A, B), (valid_assertions(A,B), memberchk(B, BL))).

comp_assrt(exports, _) --> [].
comp_assrt(inner, BL) -->
	findall((A, B), (valid_assertions(A,B), memberchk(B, BL))).
% ----------------------------------------------------------------------------

generate_callloc2(Dict, PLoc, PosLocs, Pred, NameFmt, Body0, Body) :-
	get_predname(NameFmt, Dict, Pred, PredName),
	push_term(PLoc, PosLocs, Loc),
	!,
	put_call_stack(Body, callloc(PredName, Loc), Body0).

generate_callloc(predicate, (yes, _), Dict, Loc, PosLocs, Pred, NameFmt) -->
	!,
	generate_callloc2(Dict, Loc, PosLocs, Pred, NameFmt).
generate_callloc(_, _, _, _, _, _, _) --> [].

generate_rtchecks(Assrs, Pred, M, PDict, PLoc, UsePosLoc, Lits, G) :-
	generate_step1_rtchecks(Assrs, Pred, M, PLoc, UsePosLoc, G0, G1),
	generate_step2_rtchecks(Assrs, Pred, M, PDict, PLoc, UsePosLoc, G1, G),
	lists_to_lits(G0, Lits),
	!.

%% Trivial abstraction: Check for compatibility issues in properties,
%% compatibility is an abstraction that makes static check decidable.
%% The pay-off is a lost of precision. TBD: Formal demostration. --EMM
abstract_assertions(assr(Pred, Status, Type, Compat0, Call, Succ, _Comp,
			 Loc, PredName, CompatName0, CallName, SuccName, _CompName, Dict),
		    assr(Pred, Status, Type, Compat, [], [], [],
			 Loc, PredName, CompatName, [], [], [], Dict)) :-
    append([CompatName0, CallName, SuccName], CompatName),
    append([Compat0, Call, Succ], Compat).

% Generate compile-time checks, currently only compatibility is checked,
% fails if no ctchecks can be applied to Pred
:- export(generate_ctchecks/4).
generate_ctchecks(Pred, M, Loc, Lits) :-
    collect_assertions(Pred, M, ctcheck, Assertions0),
    maplist(abstract_assertions, Assertions0, Assertions),
				% Abstraction step, here we lose precision
				% but we gain computability of checks at
				% earlier, even compile-time. --EMM
    current_prolog_flag(rtchecks_asrloc,  UseAsrLoc),
    current_prolog_flag(rtchecks_predloc, UsePredLoc),
    compat_rtchecks(Assertions, Pred, Loc, (UsePredLoc, UseAsrLoc), PosLocs0, _,
		    [], _, Goal1, []),
    once(reverse(PosLocs0, PosLocs1)),
    collapse_terms(Goal1, PosLocs1, PosLocs2),
    reverse(PosLocs2, PosLocs),
    append(PosLocs, Goal1, Goal0),
    Goal0 \= [],
    lists_to_lits(Goal0, Lits).

generate_rtchecks(F, A, M, Assrs, Pred, PDict, PLoc, UsePosLoc, Pred2,
		  Head, Body0, Body) -->
	{generate_step1_rtchecks(Assrs, Pred, M, PLoc, UsePosLoc, Body0, Body01)},
	( {Body0 \== Body01} ->
	  { rename_head('1', A, Pred, Pred1),
	    record_goal_alias(Pred, Pred1, M),
	    Body01 = Pred1,
	    lists_to_lits(Body0, Lits0)
	  },
	  add_use_inline(F, A),
	  [(Pred :- Lits0)]
	; {Pred = Pred1}
	),
	{generate_step2_rtchecks(Assrs, Pred, M, PDict, PLoc, UsePosLoc,
				 Body1, Body12)},
	( {Body1 \== Body12} ->
	  ( { var(Body0) ; Body0 \= '$orig_call'(_) } ->
	    { rename_head('2', A, Pred, Pred2),
	      Body12 = Pred2,
	      functor(Pred1, F1, A1)
	    },
	    add_use_inline(F1, A1)
	  ; { Head = Pred,
	      Body12 = Body
	    }
	  ),
	  {lists_to_lits(Body1, Lits1)},
	  [(Pred1 :- Lits1)]
	; {Pred1 = Pred2}
	),
	( { var(Body0) ; Body0 \= '$orig_call'(_) } ->
	  { record_head_alias(Pred, Pred2, M), % TODO: Optimize this literal
	    head_alias_db(Head, Head1, M)
	  },
	  [(Head1 :- Body)]
	; []
	).

current_assertion_2(Pred0, TimeCheck, Status, Type, Pred, Compat,
		    Call, Succ, Comp, Dict0, S, LB, LE, F, A, M) :-
	assertion_read(Pred0, M, Status, Type, ABody, Dict0, S, LB, LE),
	assertion_body(Pred, Compat, Call, Succ, Comp, _Comm, ABody),
	assertion_is_valid(TimeCheck, Status, Type, Comp),
	functor(Pred, F, CA),
	lit_clause_arity(M, F, A, CA),
	\+ inline_db(F, A, _, M),
	( current_prolog_flag(rtchecks_level, inner) -> true
	; current_prolog_flag(rtchecks_level, exports),
	  exported_predicate(M:F/A) -> true
	),
	\+ black_list_pred(F, A).

remaining_pred(F, A, M) :-
	current_assertion_2(_, rtcheck, _, _, _, _, _, _, _, _, _, _, _, F, A, M),
	\+ generated_rtchecks_db(F, A, M).

current_assertion(Pred0, M, TimeCheck,
		  assr(Pred, Status, Type, Compat, Call, Succ, Comp, Loc, PredName,
		       CompatName, CallName, SuccName, CompName, Dict)) :-
	working_directory(Dir, Dir),
	current_assertion_2(Pred0, TimeCheck, Status, Type, Pred, Compat, Call,
			    Succ, Comp0, Dict0, AS, LB, LE, _F, _A, M),
	rel_file_name(AS, S),
	Loc = loc(S, LB, LE),
	collapse_dups(Comp0, Comp),
	current_prolog_flag(rtchecks_namefmt, NameFmt),
	get_pretty_names(NameFmt, n(Pred, Compat, Call, Succ, Comp), Dict0, TermName, Dict),
	TermName = n(PredName, CompatName, CallName, SuccName, CompName).

assertion_pred(assr(Pred, _, _, _, _, _, _, _, _, _, _, _, _, _), Pred).

collect_assertions(Pred, M, TimeCheck, Assertions) :-
	findall(Assertion, current_assertion(Pred, M, TimeCheck, Assertion), Assertions),
	list(Assertions, assertion_pred(Pred)).

pre_lit(pre(ChkProp, Prop, _, PropValues), cui(Prop - [], PropValues, ChkProp)).

pre_fails(pre(_, _, _, PropValues), cui(PropValues, _, (PropValues \= []))).

pre_error(pre(_, _, Error, PropValues), cui(PropValues, _, Error)).

compat_rtcheck(assr(Pred, Status, Type, Compat, _, _, _, ALoc, PName, CompatNames, _, _, _, Dict),
	       UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	       pre(ChkCompat, Compat,
		   send_rtcheck(PropValues, compat, PredName, Dict, PosLoc),
		   PropValues)) :-
	memberchk((Status, Type), StatusTypes),
	\+(Compat == []),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_checkc(compat, Compat, CompatNames, PropValues, ChkCompat).

compat_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs,
	    StatusTypes, CheckedL0, CheckedL) -->
	{collect_checks(Assertions, compat_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), ChkCalls)},
	body_check_pre(ChkCalls, pre_lit, pre_fails, pre_error, collapse_prop,
	    CheckedL0, CheckedL).

calls_rtcheck(assr(Pred, Status, Type, _, Call, _, _, ALoc, PName, _, CallNames, _, _, Dict),
	      UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	      pre(ChkCall, Call,
		  send_rtcheck(PropValues, calls, PredName, Dict, PosLoc),
		  PropValues)) :-
	memberchk((Status, Type), StatusTypes),
	\+(Call == []),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_checkc(call, Call, CallNames, PropValues, ChkCall).

calls_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs, StatusTypes,
	    CheckedL0, CheckedL) -->
	{collect_checks(Assertions, calls_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), ChkCalls)},
	body_check_pre(ChkCalls, pre_lit, pre_fails, pre_error, collapse_prop,
	    CheckedL0, CheckedL).

success_call_lit(succ(ChkCall, Call, PropValues, _, _),
	    cui(Call - [], PropValues, ChkCall)).

success_succ_lit(succ(_, _, PropValues, ChkSucc, Succ),
	    cui(Succ - PropValues, _, ChkSucc)).

:- pred success_rtchecks/10 + not_fails.

success_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs, StatusTypes,
	    CheckedL0, CheckedL) -->
	{collect_checks(Assertions, success_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), CheckSuccs)},
	body_check_pos(CheckSuccs, success_call_lit, success_succ_lit,
	    collapse_prop, pos(Pred, success), CheckedL0,
	    CheckedL).

check_poscond(PosLoc, PredName, Dict, Prop, PropNames, PropValues,
	    i(PosLoc, PredName, Dict, Prop, PropNames, PropValues)).

success_rtcheck(
	    assr(Pred, Status, Type, _, Call, Succ, _, ALoc, PName, _, _, SuccNames, _, Dict),
	    UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	    succ(ChkCall, Call, PropValues, ChkSucc, Succ)) :-
	member((Status, Type), StatusTypes),
	\+(Succ == []),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_checkc(call, Call, PropValues, ChkCall),
	check_poscond(PosLoc, PredName, Dict, Succ, SuccNames, PropValues, ChkSucc).

compatpos_compat_lit(compatpos(ChkCompat, _, Compat, PropValues),
	    cui(Compat - [], PropValues, ChkCompat)).

compatpos_lit(compatpos(_, ChkCompatPos, Compat, PropValues),
	    cui(Compat - PropValues, _, ChkCompatPos)).

compatpos_rtchecks(Assertions, Pred, PLoc, UsePosLoc, PosLocs, StatusTypes,
	    CheckedL0) -->
	{collect_checks(Assertions, compatpos_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), CheckSuccs)},
	body_check_pos(CheckSuccs, compatpos_compat_lit, compatpos_lit,
	    collapse_prop, pos(Pred, compatpos), CheckedL0, _).

compatpos_rtcheck(
	    assr(Pred, Status, Type, Compat, _, _, _, ALoc, PName, CompatNames, _, _, _, Dict),
	    UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	    compatpos(ChkCompat, ChkCompatPos, Compat, PropValues)) :-
	member((Status, Type), StatusTypes),
	\+(Compat == []),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	get_checkc(compat, Compat, PropValues, ChkCompat),
	check_poscond(PosLoc, PredName, Dict, Compat, CompatNames, PropValues,
	    ChkCompatPos).

:- pred collapse_dups(+list, ?list) # "Unifies duplicated terms.".

collapse_dups([],            []).
collapse_dups([Comp|Comps0], Comps) :-
	collapse_dups2(Comp, Comps0, Comps).

collapse_dups2(Comp0, Comps0, Comps) :-
	select(Comp, Comps0, Comps1),
	Comp==Comp0 ->
	collapse_dups2(Comp, Comps1, Comps)
    ;
	collapse_dups3(Comps0, Comp0, Comps).

collapse_dups3([],             Comp, [Comp]).
collapse_dups3([Comp0|Comps0], Comp, [Comp|Comps]) :-
	collapse_dups2(Comp0, Comps0, Comps).

comps_to_comp_lit(PropValues, Comp, Body0, Body) :-
	comps_parts_to_comp_lit(PropValues, Comp, Body1, Body),
	lists_to_lits(Body1, Body0).

valid_commands([times(_, _), try_sols(_, _)]).

comps_parts_to_comp_lit(PropValues, Comp0, Body0, Body) :-
	valid_commands(VC),
	difference(Comp0, VC, Comp),
	comps_to_goal(Comp, Body1, Body2),
	( Body1 == Body2 ->
	  Body0 = Body
	; PropValues == [] ->
	  Body2 = Body,
	  Body0 = Body1
	; Body0 = checkif_comp(PropValues, Body1, Body2, Body)
	).

:- use_module(library(implementation_module)).

comp_no_signal(Comp, M) :-
	implementation_module(M:Comp, IM),
	assrt_lib:assertion_db(Comp, IM, true, prop, [], [], [], [no_signal], _, _, _).

get_chkcomp(Comp, PropValues, Pred, M, PredName, Dict, PosLoc, Body0, Body) :-
	comps_to_comp_lit(PropValues, Comp, Body1, Body),
	( PosLoc \= [],
	  \+ maplist(comp_no_signal(M), Comp)
	->Body0 = add_info_rtsignal(Body1, PropValues, Pred,
				    PredName, Dict, PosLoc)
	; Body0 = Body1 % PredName and Dict are less relevant than PosLoc
	).

comp_rtcheck(assr(Pred, Status, Type, _, Call, _, Comp, ALoc, PName, _,
		  _CallNames, _, CompNames, Dict),
	    UsePosLoc, Pred, PLoc, PosLocs, StatusTypes,
	    comp(ChkCall, Call, PropValues, ChkComp, Comp)) :-
	member((Status, Type), StatusTypes),
	\+(Comp == []),
	get_checkc(call, Call, PropValues, ChkCall),
	insert_posloc(UsePosLoc, PName, PLoc, ALoc, PosLocs, PredName, PosLoc),
	check_poscond(PosLoc, PredName, Dict, Comp, CompNames, PropValues, ChkComp).

comp_call_lit(comp(ChkCall, Call, PropValues, _, _),
	      cui(Call - [], PropValues, ChkCall)).

comp_comp_lit(comp(_, _, PropValues, ChkComp, Comp), cui(Comp - PropValues, _, ChkComp)).

compound_comp(Goal0-Goal, Goal0, Goal).

:- discontiguous body_check_comp/7.
body_check_comp([],       _,         _,        _,    _, Body,  Body) :- !.
body_check_comp(ChkComps, CheckedL0, GlobName, Pred, M, Body0, Body) :-
	compound_rtchecks_end(comp_call_lit, collapse_prop,
	    ChkComps, CheckedL0, CompCall),
	compound_rtchecks_end(comp_comp_lit, collapse_prop,
	    ChkComps, [],        CompCompL),
	map(CompCompL, comp_to_lit(GlobName, M), ChkComp0),
	sort(ChkComp0, ChkComp1),
	comps_to_goal([with_goal(G, Pred)-G|ChkComp1], compound_comp, CompBody, Body),
	Body0 = [CompCall, CompBody].

comp_rtchecks(Assertions, Pred, M, PLoc, UsePosLoc, PosLocs, StatusTypes,
	    CheckedL) -->
	{collect_checks(Assertions, comp_rtcheck(UsePosLoc, Pred,
		    PLoc, PosLocs, StatusTypes), ChkComps),
	 current_prolog_flag(rtchecks_namefmt, NameFmt),
	 get_globname(NameFmt, Pred, GlobName)},
	body_check_comp(ChkComps, CheckedL, GlobName, Pred, M).

comp_to_lit(i(PosLoc, PredName, Dict, Comp, _CompNames, PropValues), GlobName, M, ChkComp-Goal) :-
	get_chkcomp(Comp, PropValues, GlobName, M, PredName, Dict, PosLoc, ChkComp, Goal).
