:- module(assrt_meta, []).

:- use_module(library(location_utils)).
:- use_module(library(assertions_op)).
:- use_module(library(swi/assertions)).
:- use_module(library(rtchecks/rtchecks_basic)).
:- use_module(library(rtchecks/rtchecks_gen)).

:- create_prolog_flag(assrt_meta_pred, check, [type(atom)]).

% Extends assertion_db/11 to get assertions from meta predicate declarations.

assrt_lib:assertion_db(Head, M, Status, (comp), [], [], [],
		       [assrt_meta:rtc_stub(RTChecks, Goal)], "", [], Pos) :-
    current_prolog_flag(assrt_meta_pred, Status),
    Status \= none,
    Pred = M:Head,
    ( var(Head)
    ->current_predicate(M:F/A),
      functor(Head, F, A)
    ; true
    ),
    \+ predicate_property(Pred, imported_from(_)),
    % if something can not be debugged, can not be rtchecked either
    \+ predicate_property(Pred, nodebug),
    '$predicate_property'(meta_predicate(Spec), Pred),
    % predicate_property(Pred, meta_predicate(Spec)),
    ( property_from(M:Spec, meta_predicate, Pos) -> true
    ; predicate_from(Pred, Pos)
    ),
    assertion(nonvar(Pos)),
    normalize_assertion_head(Spec, M, _, Pred, Comp, Call, Succ, Glob, _),
    current_prolog_flag(rtchecks_namefmt, NameFmt),
    get_pretty_names(NameFmt, n(Head, Comp, Call, Succ, Glob), [], TName, Dict),
    TName = n(HeadName, CompName, CallName, SuccName, GlobName),
    AssrL = [assr(Head, Status, (pred),
		  Comp, Call, Succ, Glob, Pos, HeadName,
		  CompName, CallName, SuccName, GlobName, Dict)],
    generate_rtchecks(AssrL, Head, M, Pos, RTChecksL, G, G, Goal),
    lists_to_lits(RTChecksL, RTChecks).

:- true prop rtc_stub/3.
:- meta_predicate rtc_stub(0,0,?).
:- public rtc_stub/3.

rtc_stub(Goal, RTChecks, Goal) :-
    call(RTChecks).
