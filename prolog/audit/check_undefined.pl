:- module(check_undefined, []).

% A wrapper from library(check) to tools(audit)
:- use_module(library(prolog_codewalk)).
:- use_module(library(location_utils)).
:- use_module(library(extra_location)).
:- use_module(library(option_utils)).
:- use_module(library(referenced_by)).
:- use_module(library(check), []).
:- use_module(library(audit/audit)).

:- multifile
    prolog:message//1.

audit:check(undefined, Results, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_undefined(from_chk(FileChk), OptionL, Results).

:- meta_predicate check_undefined(1,+,-).
check_undefined(FromChk, OptionL0, Pairs) :-
    select_option(module(M), OptionL0, OptionL1, M),
    merge_options(OptionL1,
		  [source(false),
		   infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   undefined(trace),
		   %% module_class([user, system, library]),
		   on_trace(collect_undef(M, FromChk))],
		  OptionL),
    prolog_walk_code(OptionL),
    forall(( extra_location(Head, CM, goal, From),
	     functor(Head, F, A),
	     \+ current_predicate(CM:F/A)
	   ),
	   ignore(collect_undef(M, FromChk, CM:Head, _, From))),
    findall(warning-(PIAL-(Loc/CI)),
	    ( retract(check:undef(PI, From)),
	      find_alternatives(PI, AL),
	      PIAL=PI/AL,
	      from_location(From, Loc),
	      check:predicate_indicator(From, CI, [])
	    ), Pairs).

hide_undef(M:H) :- hide_undef(H, M).

find_alternatives(M:F/A, AL) :-
    functor(H, F, A),
    findall(AM, ( current_predicate(AM:F/A),
		  AM \= M,
		  \+ predicate_property(AM:H, imported_from(_))
		), AU),
    sort(AU, AL).

% Hook to hide undef messages:
:- multifile hide_undef/2.
hide_undef(assertion_head(_,_,_,_,_,_,_), assrt_lib).

found_undef(To, _Caller, From) :-
    check:goal_pi(To, PI),
    ( hide_undef(To) -> true
    ; check:undef(PI, From) -> true
    ; assertz(check:undef(PI, From))
    ).

:- public collect_undef/5.
:- meta_predicate collect_undef(?,1,+,+,+).
collect_undef(M, FromChk, MCall, Caller, From) :-
    call(FromChk, From),
    M:_ = MCall,
    found_undef(MCall, Caller, From),
    fail. % prevent unexpected unification

prolog:message(acheck(undefined)) -->
    ['--------------------',nl,
     'Undefined Predicates',nl,
     '--------------------',nl],
    prolog:message(check(undefined_predicates)).
prolog:message(acheck(undefined, PIAL-LocCIList)) -->
    { PIAL = PI/AL
    ->true
    ; PI = PIAL,
      AL = []
    },
    check:predicate(PI),
    [ ' undefined, ' ],
    show_alternatives(AL),
    [ 'referenced by', nl ],
    referenced_by(LocCIList).

show_alternatives([]) --> !.
show_alternatives(AL) --> ['but modules ~w have definitions for it, '-[AL]].
