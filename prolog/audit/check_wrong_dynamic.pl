:- module(check_wrong_dynamic, []).

:- use_module(library(compact_pi_list)).
:- use_module(tools(tools_common)).
:- use_module(library(database_fact)).
:- use_module(library(location_utils)).
:- use_module(library(check), []).

:- multifile
    prolog:message//1,
    audit:check/4,
    hide_wrong_dynamic/1,
    hide_var_dynamic/1.

:- dynamic
    wrong_dynamic_db/3,
    check_var_dynamic_db/3.

hide_wrong_dynamic(user:prolog_trace_interception/4).

cleanup_dynamic_db :-
    retractall(wrong_dynamic_db(_, _, _)),
    retractall(check_var_dynamic_db(_, _, _)),
    cleanup_locations(_, dynamic(_, _), _).

audit:check(wrong_dynamic, Ref, Result, OptionL) :-
    option_allchk(OptionL, _, FileChkL),
    check_wrong_dynamic(Ref, collect_wrong_dynamic(FileChkL), Result).

:- meta_predicate check_wrong_dynamic(?,3,-).
check_wrong_dynamic(Ref0, Collect, Pairs) :-
    cleanup_dynamic_db,
    normalize_head(Ref0, Ref),
    prolog_walk_code([infer_meta_predicates(false),
		      autoload(false),
		      evaluate(false),
		      trace_reference(Ref),
		      on_trace(Collect)]),
    collect_result(Ref, Pairs),
    cleanup_dynamic_db.

collect_result(Ref, Pairs) :-
    findall(Type-(static_as_dynamic-((Loc/PI)-(MLoc/MPI))),
	    current_static_as_dynamic(Type, Loc, PI, MLoc, MPI), Pairs, Pairs1),
    findall(warning-(dynamic_as_static-(Loc-PI)),
	    current_dynamic_as_static(Ref, Loc, PI), Pairs1, Pairs2),
    findall(warning-(var_as_dynamic-(PI-(Loc/CI))),
	    (retract(check_var_dynamic_db(PI, CI, From)),
	     from_location(From, Loc)), Pairs2, []).

current_static_as_dynamic(Type, Loc, PI, MLoc, MPI) :-
    wrong_dynamic_db(TypeDB, PI, MPI-MFrom),
    memberchk(TypeDB,[def,retract]),
    PI = M:F/A,
    functor(H,F,A),
    Ref = M:H,
    \+ predicate_property(Ref, dynamic),
    \+ predicate_property(Ref, volatile),
    ( predicate_property(Ref, number_of_clauses(N)),
      N > 0 ->
      Type = error,
      predicate_location(Ref, Loc)
    ; Type = warning,
      once(property_location(PI, _, Loc))      
    ),
    from_location(MFrom, MLoc).

current_dynamic_as_static(Ref, Loc, PI) :-
    Ref = M:H,
    PI = M:F/A,
    ( var(H) ->
      current_defined_predicate(PI),
      functor(H, F, A)
    ; functor(H, F, A),
      current_defined_predicate(PI)
    ),
    auditable_predicate(Ref),
    predicate_property(Ref, dynamic),
    \+ ( wrong_dynamic_db(Type, PI, _),
	 memberchk(Type,[def,retract])
       ),
    property_location(PI, dynamic, Loc).

prolog:message(acheck(wrong_dynamic, Type-List)) -->
    wrong_dynamic_message(Type, List).

static_as_dynamic(Loc/PI-MLocPIs) -->
    ['\t'|Loc], ['~w modified by'-[PI], nl],
    maplist_dcg(show_locpi, MLocPIs).

show_locpi(Loc/PI) --> ['\t\t'|Loc], check:predicate(PI), [nl].

show_locci(Loc/CI) --> ['\t\t'|Loc], CI, [nl].

dynamic_as_static(Loc-PIs) -->
    {compact_pi_list(PIs, CPIs)},
    ['\t'|Loc], ['predicates ~w'-[CPIs], nl].

wrong_dynamic_message(static_as_dynamic, LocPIs) -->
    ['Predicates never declared dynamic, but modified:', nl],
    maplist_dcg(static_as_dynamic, LocPIs).
wrong_dynamic_message(dynamic_as_static, LocPIs) -->
    ['Predicates declared dynamic, but never modified:', nl],
    maplist_dcg(dynamic_as_static, LocPIs).
wrong_dynamic_message(var_as_dynamic, PILocCIs) -->
    ['Predicates called with a variable in a module-sensitive argument:', nl],
    maplist_dcg(var_as_dynamic, PILocCIs).

var_as_dynamic(PI-LocCIs) -->
    ['\t~w called with a variable in'-[PI], nl],
    maplist_dcg(show_locci, LocCIs).

prolog:message(acheck(wrong_dynamic)) -->
    ['--------------------------', nl,
     'Wrong Dynamic Declarations', nl,
     '--------------------------', nl,
     'The predicates below present inconsistencies between its', nl,
     'usage and the dynamic declarations. Could be that they are', nl,
     'being used as dynamic without a proper declaration, being', nl,
     'declared as dynamic but never asserted, retracted, or using', nl,
     'a variable argument in a database predicate, making it', nl,
     'difficult to analyze.', nl, nl].

:- meta_predicate collect_wrong_dynamic(+,:,+,+).
collect_wrong_dynamic(FileChkL, MGoal, Caller, From) :-
    from_to_file(From, File),
    forall(member(FileChk, FileChkL), call(FileChk, File)),
    collect_wrong_dynamic(MGoal, Caller, From),
    fail.
collect_wrong_dynamic(_, _, _, _). % avoid side effects

collect_wrong_dynamic(MGoal, Caller, From) :-
    record_location_meta(MGoal, From, database_fact_ort,
			 record_location_wd(Caller)).

record_location_wd(Caller, M:Fact, Def, From) :-
    Def = dynamic(Type, MGoal),
    normalize_pi(MGoal, MPI),
    ( nonvar(M),
      nonvar(Fact)
    ->functor(Fact, F, A),
      record_location(M:F/A, Def, From),
      \+ hide_wrong_dynamic(M:F/A),
      assertz(wrong_dynamic_db(Type, M:F/A, MPI-From))
    ; \+ database_fact(Caller) ->
      normalize_pi(Caller, HPI),
      \+ hide_var_dynamic(HPI),
      check:predicate_indicator(From, HCI, []),
      assertz(check_var_dynamic_db(MPI, HCI, From))
    ; true
    ).
