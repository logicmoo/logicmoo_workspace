:- module(check_trivial_fails, []).

:- use_module(library(prolog_codewalk)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).
:- use_module(library(option_utils)).
:- use_module(library(extra_location)).
:- use_module(library(maplist_dcg)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(audit/audit)).

:- multifile
    prolog:message//1.

:- dynamic trivial_fail/2.

audit:check(trivial_fails, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_trivial_fails(from_chk(FileChk), OptionL, Result).

check_trivial_fails(FromChk, OptionL0, Pairs) :-
    select_option(module(M), OptionL0, OptionL1, M),
    merge_options(OptionL1,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_)
		  ], OptionL),
    prolog_walk_code([source(false),
		      on_trace(collect_trivial_fail_1r(M, FromChk))
		     |OptionL]),
    prolog_walk_code([source(false),
		      on_trace(collect_trivial_fail_2r(M, FromChk))
		     |OptionL]),
    findall(CRef, retract(trivial_fail(clause(CRef), _)), ClausesU),
    sort(ClausesU, Clauses),
    ( Clauses==[]
    ->Pairs=[]
    ; prolog_walk_code([clauses(Clauses),
			on_trace(collect_trivial_fail(M))
		       |OptionL]),
      findall(warning-(Loc-Args),
	      ( retract(trivial_fail(From, Args)),
		from_location(From, Loc)
	      ), Pairs)
    ),
    !.

prolog:message(acheck(trivial_fails)) -->
    ['-------------',nl,
     'Trivial Fails',nl,
     '-------------',nl,
     'The literals below always fails, due to there are no',nl,
     'matching clauses for such calls', nl, nl].
prolog:message(acheck(trivial_fails, Loc-Args)) -->
    Loc,
    maplist_dcg(show_trivial_fail, Args).

show_trivial_fail(Arg) -->
    ['In ~q, trivial fail for ~q'-Arg, nl].

:- multifile ignore_predicate/1.
ignore_predicate(pce_expansion:pce_class(_, _, template, _, _, _)).
ignore_predicate(pce_host:property(system_source_prefix(_))).
ignore_predicate(pce_expansion:verbose).

:- public
    collect_trivial_fail_1r/5,
    collect_trivial_fail_2r/5.

:- meta_predicate collect_trivial_fail_1r(+,1,+,+,+).
collect_trivial_fail_1r(M, FromChk, MGoal, _, From) :-
    nonvar(MGoal),
    call(FromChk, From),
    record_location_dynamic(MGoal, M, From).

:- meta_predicate collect_trivial_fail_2r(+,1,+,+,+).
collect_trivial_fail_2r(M, FromChk, MGoal, Caller, From) :-
    nonvar(MGoal),
    call(FromChk, From),
    collect_trivial_fail(M, MGoal, Caller, From).

collect_trivial_fail(M, MCall, Caller, From) :-
    record_location_meta(MCall, M, From, all_call_refs, cu_caller_hook(Caller)).

:- use_module(library(abstract_interpreter)).

cu_caller_hook(Caller, M:H, CM, _, _, _, From) :-
    atom(CM),
    callable(H),
    ( predicate_property(CM:H, interpreted),
      %% \+ predicate_property(CM:H, dynamic),
      \+ predicate_property(CM:H, multifile),
      \+ ignore_predicate(M:H)
    ->( predicate_property(CM:H, meta_predicate(Meta)) ->
	qualify_meta_goal(CM:H, Meta, Goal)
      ; Goal = H
      ),
      MGoal = M:Goal,
      Args = [Caller, MGoal],
      ( \+ abstract_interpreter(CM:Goal, head_abstraction)
      ->( \+ ( trivial_fail(From0, Args),
	       subsumes_from(From, From0 )
	     )
	->forall(( trivial_fail(From0, Args), 
		   subsumes_from(From0, From)
		 ),
		 retract(trivial_fail(From0, Args))), % Clean up less precise facts
	  assertz(trivial_fail(From, Args))
	; true
	)
      ; true
      )
    ).

dyn_defined(M:Head) :-
    extra_location(Head, M, dynamic(def, _, _), _).
