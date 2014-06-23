:- module(check_undefined, []).

% A wrapper from library(check) to tools(audit)

:- use_module(tools(tools_common)).
:- use_module(library(location_utils)).
:- use_module(library(check), []).

:- multifile
    audit:check/4,
    prolog:message//1.

audit:check(undefined, Ref0, Results, OptionL) :-
    option_filechk(OptionL, FileChk),
    normalize_head(Ref0, Ref),
    check_undefined(collect_undef(FileChk, Ref), Results).

:- meta_predicate check_undefined(3,-).
check_undefined(Collect, Pairs) :-
    prolog_walk_code([infer_meta_predicates(false),
		      autoload(false),
		      undefined(trace),
		      evaluate(false),
		      module_class([user]),
		      on_trace(Collect)]),
    findall(warning-(PI-(Loc/CI)),
	    ( retract(check:undef(PI, From)),
	      from_location(From, Loc),
	      check:predicate_indicator(From, CI, [])
	    ), Pairs),
    cleanup_locations(_, dynamic(_, _), _).

% Hook to hide undef messages:
:- multifile hide_undef/1.

found_undef(To, _Caller, From) :-
    goal_pi(To, PI),
    ( hide_undef(To) -> true
    ; check:undef(PI, From) -> true
    ; assertz(check:undef(PI,From))
    ).

goal_pi(M:H, M:F/A) :- functor(H, F, A).

collect_undef(FileChk, Ref, Ref, Caller, From) :-
    from_to_file(From, File),
    call(FileChk, File),
    record_location_dynamic(Ref, From),
    found_undef(Ref, Caller, From),
    fail. % prevent Ref unification

prolog:message(acheck(undefined)) -->
    ['--------------------',nl,
     'Undefined Predicates',nl,
     '--------------------',nl],
    prolog:message(check(undefined_predicates)).
prolog:message(acheck(undefined, PI-LocCIList)) -->
    check:predicate(PI),
    [ ', which is referenced by', nl ],
    referenced_by(LocCIList).

% prolog:message(check(undefined(PI, LocList))).
