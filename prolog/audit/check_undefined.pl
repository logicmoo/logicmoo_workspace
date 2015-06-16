:- module(check_undefined, []).

% A wrapper from library(check) to tools(audit)
:- use_module(library(prolog_codewalk)).
:- use_module(library(infer_alias)).
:- use_module(library(location_utils)).
:- use_module(library(normalize_pi)).
:- use_module(library(referenced_by)).
:- use_module(library(audit/audit)).
:- use_module(library(audit/audit_codewalk)).
:- use_module(library(assertions/assrt_lib)).

:- multifile
    prolog:message//1.

:- dynamic
    undef/3.

audit:check(undefined, Results, OptionL) :-
    check_undefined(OptionL, Results).

check_undefined(OptionL0, Pairs) :-
    audit_wcsetup([trace_reference(-), undefined(trace)|OptionL0],
		  OptionL, M, FromChk),
    prolog_walk_code([on_trace(collect_undef(M, FromChk))|OptionL]),
    decl_walk_code(extra_undef(M, FromChk), M),
    found_undef_assr(M, FromChk),
    findall(warning-(PIAL-(Loc/['~w'-[CI]])),
	    ( retract(undef(PI, CI, From)),
	      find_alternatives(PI, AL),
	      PIAL=PI/AL,
	      from_location(From, Loc)
	    ), Pairs).

extra_undef(M, FromChk, M:Head, Caller, From) :-
    functor(Head, F, A),
    \+ current_predicate(M:F/A),
    collect_undef(M, FromChk, M:Head, Caller, From).

hide_undef(M:H) :- hide_undef(H, M).

find_alternatives(M:F/A, AL) :-
    functor(H, F, A),
    findall(AA, ( current_predicate(AM:F/A),
		  AM \= M,
		  \+ predicate_property(AM:H, imported_from(_)),
		  ( module_property(AM, file(AF))
		  ->( library_alias(AF, AA)
		    ->true
		    ; AA = AF
		    )
		  ; AA=AM
		  )
		), AU),
    sort(AU, AL).

% Hook to hide undef messages:
:- multifile hide_undef/2.
hide_undef(assertion_head(_,_,_,_,_,_,_), assrt_lib).

found_undef(To, Caller, From) :-
    normalize_pi(To, PI),
    normalize_pi(Caller, CI),
    ( hide_undef(To) -> true
    ; undef(PI, CI, From) -> true
    ; assertz(undef(PI, CI, From))
    ).

found_undef_assr(M, FromChk) :-
    forall(( assertion_head_body_loc(Head, M, _, _, _, _, _, From),
	     functor(Head, F, A),
	     \+ current_predicate(M:F/A),
	     call(FromChk, From)),
	   found_undef(M:Head, assrt_lib:assertion_head/7, From)).

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
    [ '~w undefined, '-[PI]],
    show_alternatives(AL),
    [ 'referenced by', nl ],
    referenced_by(LocCIList).

show_alternatives([]) --> !.
show_alternatives(AL) --> ['but modules ~w have definitions for it, '-[AL]].
