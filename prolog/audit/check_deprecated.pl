:- module(check_deprecated, []).

:- use_module(library(prolog_codewalk)).
:- use_module(library(check), []).
:- use_module(library(implementation_module)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(referenced_by)).
:- use_module(library(audit/audit)).

:- multifile
    prolog:message//1,
    deprecated_predicate/2.

audit:check(deprecated, Result, OptionL0) :-
    option_allchk(OptionL0, OptionL, FileChk),
    check_deprecated(from_chk(FileChk), OptionL, Result).

check_deprecated(FromChk, OptionL0, Pairs) :-
    select_option(module(M), OptionL0, OptionL1, M),
    merge_options(OptionL1,
		  [infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_)
		  ], OptionL),
    prolog_walk_code([source(false),
		      on_trace(have_deprecated(M, FromChk))
		     |OptionL]),
    findall(CRef, retract(deprecated_db(clause(CRef))), Clauses),
    ( Clauses==[]
    ->Pairs=[]
    ; prolog_walk_code([clauses(Clauses),
			on_trace(collect_deprecated(M))
		       |OptionL]),
      findall(information-(((IM:Call)/Alt)-(Loc/CI)),
	      ( retract(deprecated_db(Call, IM, Alt, From)),
		from_location(From, Loc),
		check:predicate_indicator(From, CI, [])
	      ), Pairs)
    ).

predicate_head(Module:Head) -->
    { nonvar(Head),
      arg(_, Head, Arg),
      nonvar(Arg)
    },
    !,
    ['~w'-[Module:Head]].
predicate_head(Head) -->
    check:predicate(Head).

prolog:message(acheck(deprecated)) -->
    ['---------------------',nl,
     'Deprecated Predicates',nl,
     '---------------------',nl,
     'The predicates below are marked as deprecated, so you have to', nl,
     'avoid its usage in new code, and to refactorize old code.', nl, nl].
prolog:message(acheck(deprecated, (PI/Alt)-LocCIs)) -->
    predicate_head(PI),
    [' deprecated, use ~q instead. Referenced by'-[Alt], nl],
    referenced_by(LocCIs).

:- dynamic deprecated_db/1, deprecated_db/4.

:- public have_deprecated/5.
:- meta_predicate have_deprecated(?, 1, +, +, +).

have_deprecated(M, FromChk, MGoal, _, From) :-
    call(FromChk, From),
    MGoal = _:Goal,
    implementation_module(MGoal, M),
    deprecated_predicate(M:Goal, _),
    assertz(deprecated_db(From)),
    fail.
have_deprecated(_, _, _, _, _).

:- public collect_deprecated/4.

collect_deprecated(M, MGoal, _, From) :-
    MGoal = _:Goal,
    implementation_module(MGoal, M),
    deprecated_predicate(M:Goal, Alt),
    assertz(deprecated_db(Goal, M, Alt, From)),
    fail.
collect_deprecated(_, _, _, _).
