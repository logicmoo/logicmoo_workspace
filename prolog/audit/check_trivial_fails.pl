:- module(check_trivial_fails, []).

:- use_module(library(prolog_codewalk)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(record_locations)).
:- use_module(library(maplist_dcg)).
:- use_module(library(normalize_head)).
:- use_module(library(implementation_module)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(audit/audit)).

:- multifile
    prolog:message//1.

:- dynamic trivial_fail/2.

audit:check(trivial_fails, Ref, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_trivial_fails(Ref, FileChk, OptionL, Result).

check_trivial_fails(Ref0, FileChk, OptionL0, Pairs) :-
    normalize_head(Ref0, M:H),
    merge_options(OptionL0,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_:H)
		  ], OptionL),
    prolog_walk_code([source(false),
		      on_trace(collect_trivial_fail_1r(M, FileChk))
		     |OptionL]),
    prolog_walk_code([source(false),
		      on_trace(collect_trivial_fail_2r(M, FileChk))
		     |OptionL]),
    findall(CRef, retract(trivial_fail(clause(CRef), _)), Clauses),
    ( Clauses==[]
    ->Pairs=[]
    ; prolog_walk_code([clauses(Clauses),
			on_trace(collect_trivial_fail(M))
		       |OptionL]),
      findall(warning-(Loc-Args),
	      ( retract(trivial_fail(From, Args)),
		from_location(From, Loc)
	      ), Pairs)
    ).

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
collect_trivial_fail_1r(M, FileChk, MGoal, _, From) :-
    nonvar(MGoal),
    from_to_file(From, File),
    call(FileChk, File),
    record_location_dynamic(MGoal, M, From).

:- meta_predicate collect_trivial_fail_2r(+,1,+,+,+).
collect_trivial_fail_2r(M, FileChk, MGoal, Caller, From) :-
    nonvar(MGoal),
    from_to_file(From, File),
    call(FileChk, File),
    collect_trivial_fail(M, MGoal, Caller, From).

collect_trivial_fail(M, MCall, Caller, From) :-
    record_location_meta(MCall, M, From, all_call_refs, cu_caller_hook(Caller)).

cu_caller_hook(Caller, MGoal0, _, _, _, _, From) :-
    M:H = MGoal0,
    atom(M),
    callable(H),
    ( predicate_property(MGoal0, interpreted),
      %% \+ predicate_property(MGoal0, dynamic),
      \+ predicate_property(MGoal0, multifile),
      \+ ignore_predicate(MGoal0)
    ->( predicate_property(MGoal0, meta_predicate(Meta)) ->
	qualify_meta_goal(MGoal0, Meta, MGoal)
      ; MGoal = MGoal0
      ),
      ( \+ ( clause(MGoal, _)
	   ; dyn_defined(MGoal)
	   )
      ->assertz(trivial_fail(From, [Caller, MGoal]))
      ; true
      )
    ).

dyn_defined(M:Head) :-
    implementation_module(M:Head, IM),
    extra_location(Head, IM, dynamic(def, _, _), _).
