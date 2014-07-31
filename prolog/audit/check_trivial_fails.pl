:- module(check_trivial_fails, []).

:- use_module(library(prolog_codewalk)).
:- use_module(library(location_utils)).
:- use_module(library(record_locations)).
:- use_module(library(database_fact)).
:- use_module(library(maplist_dcg)).
:- use_module(library(normalize_head)).
:- use_module(library(implementation_module)).

:- multifile
    prolog:message//1.

:- dynamic trivial_fail/2.

audit:check(trivial_fails, Ref, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_trivial_fails(Ref, FileChk, OptionL, Result).

check_trivial_fails(Ref0, FileChk, OptionL0, Pairs) :-
    normalize_head(Ref0, Ref),
    merge_options(OptionL0,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(Ref)
		  ], OptionL),
    prolog_walk_code([source(false),
		      on_trace(collect_trivial_fail_1r(FileChk))
		     |OptionL]),
    prolog_walk_code([source(false),
		      on_trace(collect_trivial_fail_2r(FileChk))
		     |OptionL]),
    findall(CRef, retract(trivial_fail(clause(CRef), _)), Clauses),
    ( Clauses==[]
    ->Pairs=[]
    ; prolog_walk_code([clauses(Clauses),
			on_trace(collect_trivial_fail)
		       |OptionL]),
      findall(warning-(Loc-Args),
	      ( retract(trivial_fail(From, Args)),
		from_location(From, Loc)
	      ), Pairs)
    ),
    cleanup_locations(_, _, dynamic(_, _), _).

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

module_qualified(:) :- !.
module_qualified(N) :- integer(N), N >= 0.

add_module(Arg, M, M:Arg) :-
    var(Arg),
    !.
add_module(M:Arg, _, MArg) :-
    !,
    add_module(Arg, M, MArg).
add_module(Arg, M, M:Arg).

meta_goal(N, M, Meta, Goal0, Goal) :-
    arg(N, Meta,  ArgM),
    !,
    arg(N, Goal0, Arg0),
    arg(N, Goal,  Arg),
    N1 is N + 1,
    ( module_qualified(ArgM) ->
      add_module(Arg0, M, Arg)
    ; Arg = Arg0
    ),
    meta_goal(N1, Meta, Goal0, Goal).
meta_goal(_, _, _, _).

:- multifile ignore_predicate/1.
ignore_predicate(pce_expansion:pce_class(_, _, template, _, _, _)).
ignore_predicate(pce_host:property(system_source_prefix(_))).

:- public
    collect_trivial_fail_1r/4,
    collect_trivial_fail_2r/4.

:- meta_predicate collect_trivial_fail_1r(1,+,+,+).
collect_trivial_fail_1r(FileChk, MGoal, _, From) :-
    nonvar(MGoal),
    from_to_file(From, File),
    call(FileChk, File),
    record_location_dynamic(MGoal, From).

:- meta_predicate collect_trivial_fail_2r(1,+,+,+).
collect_trivial_fail_2r(FileChk, MGoal, Caller, From) :-
    nonvar(MGoal),
    from_to_file(From, File),
    call(FileChk, File),
    collect_trivial_fail(MGoal, Caller, From).

collect_trivial_fail(MGoal0, Caller, From) :-
    nonvar(MGoal0 ),
    MGoal0 = M:Goal,
    atom(M),
    callable(Goal),
    implementation_module(MGoal0, IM),
    ( MGoal1 = MGoal0
    ; database_use_fact(IM:Goal, Fact0 ),
      nonvar(Fact0 ),
      ( Fact0 = FM:Fact1
      ->atom(FM),
	callable(Fact1),
	MGoal1 = Fact0
      ; callable(Fact0 )
      ->MGoal1 = M:Fact0
      )
    ),
    ( predicate_property(MGoal1, interpreted),
      %% \+ predicate_property(MGoal1, dynamic),
      \+ predicate_property(MGoal1, multifile),
      \+ ignore_predicate(MGoal1)
    ->( predicate_property(MGoal1, meta_predicate(Meta)) ->
	qualify_meta_goal(MGoal1, Meta, MGoal)
      ; MGoal = MGoal1
      ),
      ( \+ ( clause(MGoal, _)
	   ; dyn_defined(MGoal)
	   )
      ->assertz(trivial_fail(From, [Caller, MGoal0]))
      ; true
      )
    ),
    fail.

dyn_defined(M:Head) :-
    implementation_module(M:Head, IM),
    declaration_location(Head, IM, dynamic(def, _), _).
		 
qualify_meta_goal(M:Goal0, Meta, M:Goal) :-
    functor(Goal0, F, N),
    functor(Goal, F, N),
    meta_goal(1, M, Meta, Goal0, Goal).
