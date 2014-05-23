:- module(called_from, [called_from/1, called_from/2, called_from/3]).

:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(implementation_module)).
:- use_module(library(location_utils)).

:- dynamic
    record_locations:declaration_location/3.

:- multifile
    prolog:message//1,
    record_locations:declaration_location/3.

:- dynamic called_from_db/3.

prolog:message(acheck(called_from(MsgLoc, Args))) -->
	MsgLoc,
	['~w called from ~w'-Args].

called_from(Ref) :-
	called_from(Ref, _).

called_from(Ref, Caller) :-
    called_from(Ref, Caller, Sorted),
    maplist(print_call_point, Sorted),
    cleanup_locations(_, dynamic(_, _), _),
    retractall(called_from_db(_, _, _)).

called_from(Ref, Caller, Sorted) :-
    called_from_meta(Ref, collect_call_point(Caller), Sorted).

collect_called_from(Ref, OnTrace) :-
    cleanup_locations(_, dynamic(_, _), _),
    retractall(called_from_db(_, _, _)),
    setup_call_cleanup(( current_prolog_flag(verbose, Verbose),
			 set_prolog_flag(verbose, silent)
		       ),
		       prolog_walk_code([trace_reference(Ref),
					 module_class([user, system, library]),
					 infer_meta_predicates(false),
					 evaluate(false),
					 on_trace(OnTrace)]),
		       set_prolog_flag(verbose, Verbose)
		      ).

:- meta_predicate called_from_meta(?,3,-).
called_from_meta(Ref0, OnTrace, Sorted) :-
    normalize_head(Ref0, Ref),
    collect_called_from(Ref, OnTrace),
    findall(L-Args, current_called_from(Ref, L, Args), Pairs),
    keysort(Pairs, Sorted).

current_called_from(Call, Loc, [PI, CPI]) :-
    ( called_from_db(From, Call, Caller),
      normalize_pi(Call, PI),
      normalize_pi(Caller, CPI)
    ; Call = M:H,
      nonvar(H),
      functor(H, F, A),
      PI = M:F/A,
      record_locations:declaration_location(PI, dynamic(Type, CPI), From),
      Type \= def
    ),
    from_location(From, Loc).

collect_call_point(Caller, MGoal, Caller, From) :-
    implementation_module(MGoal, IM),
    record_location_dynamic(MGoal, From),
    MGoal = _M:Goal,
    assertz(called_from_db(From, IM:Goal, Caller)).

print_call_point(L-A) :-
	print_message(information, acheck(called_from(L, A))).
