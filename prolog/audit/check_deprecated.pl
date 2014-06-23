:- module(check_deprecated, []).

:- use_module(library(auditable_predicate)).
:- use_module(library(current_defined_predicate)).
:- use_module(library(implementation_module)).
:- use_module(library(normalize_head)).
:- use_module(library(location_utils)).
:- use_module(library(referenced_by)).
:- use_module(library(check), []).

:- multifile
    prolog:message//1,
    deprecated_predicate/2,
    audit:check/4.

audit:check(deprecated, Ref, Result, OptionL) :-
    option_allchk(OptionL, _, FileChk),
    check_deprecated(Ref, collect_deprecated(FileChk), Result).

:- meta_predicate check_deprecated(?,3,-).
check_deprecated(Ref0, Collect, Pairs) :-
    normalize_head(Ref0, Ref1),
    Opts = [infer_meta_predicates(false),
	    autoload(false),
	    evaluate(false),
	    trace_reference(Ref),
	    on_trace(Collect)|Opts0
	   ],
    ( var(Ref1) ->
      Ref = Ref1,
      Opts0 = []
    ; Ref1 = M:H,
      Ref  = _:H,
      Opts0 = [module(M)]
    ),
    findall(information-(Call/Alt-(Loc/CI)),
	    ( clause(deprecated_predicate(Ref, _), _), % To speed up process
	      prolog_walk_code(Opts),
	      retract(deprecated_db(Call, Alt, From)),
	      from_location(From, Loc),
	      check:predicate_indicator(From, CI, [])
	    ), Pairs),
    cleanup_locations(_, dynamic(_, _), _).

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
prolog:message(acheck(deprecated, PI/Alt-LocCIs)) -->
    predicate_head(PI),
    [' deprecated, use ~q instead. Referenced by'-[Alt], nl],
    referenced_by(LocCIs).

:- dynamic deprecated_db/3.

collect_deprecated(FileChk, M:Goal, _, From) :-
    from_to_file(From, File),
    call(FileChk, File),
    implementation_module(M:Goal, IM),
    deprecated_predicate(IM:Goal, Alt),
    assertz(deprecated_db(IM:Goal, Alt, From)).
