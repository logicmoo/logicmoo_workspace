:- module(check_undefined, []).

% A wrapper from library(check) to tools(audit)
:- use_module(library(prolog_codewalk)).
:- use_module(library(location_utils)).
:- use_module(library(normalize_head)).
:- use_module(library(referenced_by)).
:- use_module(library(check), []).

:- multifile
    audit:check/4,
    prolog:message//1.

audit:check(undefined, Ref, Results, OptionL) :-
    option_allchk(OptionL, _, FileChk),
    check_undefined(Ref, FileChk, OptionL, Results).

:- meta_predicate check_undefined(?,1,+,-).
check_undefined(Ref, FileChk, OptionL0, Pairs) :-
    normalize_head(Ref, M:H),
    merge_options(OptionL0,
		  [source(false),
		   infer_meta_predicates(false),
		   autoload(false),
		   undefined(trace),
		   evaluate(false),
		   %% module_class([system, library, user]),
		   on_trace(collect_undef(H, M, FileChk))],
		  OptionL),
    prolog_walk_code(OptionL),
    findall(warning-(PI-(Loc/CI)),
	    ( retract(check:undef(PI, From)),
	      from_location(From, Loc),
	      check:predicate_indicator(From, CI, [])
	    ), Pairs),
    cleanup_locations(_, _, dynamic(_, _, _), _).

% Hook to hide undef messages:
:- multifile hide_undef/1.

found_undef(To, _Caller, From) :-
    goal_pi(To, PI),
    ( hide_undef(To) -> true
    ; check:undef(PI, From) -> true
    ; assertz(check:undef(PI, From))
    ).

goal_pi(M:H, M:F/A) :- functor(H, F, A).

:- public collect_undef/6.
:- meta_predicate collect_undef(?,?,1,+,+,+).
collect_undef(H, M, FileChk, MCall, Caller, From) :-
    from_to_file(From, File),
    call(FileChk, File),
    M:H=MCall,
    record_location_dynamic(MCall, M, From),
    found_undef(MCall, Caller, From),
    fail. % prevent unexpected unification

prolog:message(acheck(undefined)) -->
    ['--------------------',nl,
     'Undefined Predicates',nl,
     '--------------------',nl],
    prolog:message(check(undefined_predicates)).
prolog:message(acheck(undefined, PI-LocCIList)) -->
    check:predicate(PI),
    [ ' undefined, which is referenced by', nl ],
    referenced_by(LocCIList).
