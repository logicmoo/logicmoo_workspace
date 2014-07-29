:- module(called_from, [called_from/1,
			called_from/2,
			called_from/4,
			collect_called_from/3,
			collect_called_from/4]).

:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(implementation_module)).
:- use_module(library(location_utils)).
:- use_module(library(record_locations)).

:- multifile
    prolog:message//1.

:- dynamic called_from_db/3.

prolog:message(acheck(called_from(MsgLoc, Args))) -->
	MsgLoc,
	['~w called from ~w'-Args].

called_from(Ref) :-
	called_from(Ref, _).

called_from(Ref, Caller) :-
    called_from(Ref, Caller, [], Sorted),
    maplist(print_call_point, Sorted),
    cleanup_locations(_, _, dynamic(_, _), _),
    retractall(called_from_db(_, _, _)).

called_from(Ref0, Caller, OptionL, Pairs) :-
    normalize_head(Ref0, Ref),
    collect_called_from(Ref, Caller, OptionL, Pairs).

collect_called_from(Ref, Caller, OptionL, Sorted) :-
    collect_called_from(Ref, Caller, OptionL),
    findall(L-[PI, CPI], current_called_from(Ref, L, PI, CPI), Pairs),
    keysort(Pairs, Sorted).

collect_called_from(Ref, Caller, OptionL0) :-
    cleanup_locations(_, _, dynamic(_, _), _),
    retractall(called_from_db(_, _, _)),
    merge_options([infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(Ref),
		   module_class([user, system, library]),
		   on_trace(collect_call_point(Caller))],
		  OptionL0, OptionL),
    setup_call_cleanup(( current_prolog_flag(verbose, Verbose),
			 set_prolog_flag(verbose, silent)
		       ),
		       prolog_walk_code(OptionL),
		       set_prolog_flag(verbose, Verbose)
		      ).

current_called_from(Call, Loc, PI, CPI) :-
    ( called_from_db(From, Call, Caller),
      normalize_pi(Call, PI),
      normalize_pi(Caller, CPI)
    ; Call = M:H,
      PI = M:F/A,
      ( callable(H)
      ->functor(H, F, A),
	declaration_location(H, M, dynamic(Type, CPI), From),
	Type \= def
      ; declaration_location(H, M, dynamic(Type, CPI), From),
	Type \= def,
	functor(H, F, A)
      )
    ),
    from_location(From, Loc).

:- public collect_call_point/4.
collect_call_point(Caller, MGoal, Caller, From) :-
    implementation_module(MGoal, IM),
    record_location_dynamic(MGoal, From),
    MGoal = _M:Goal,
    assertz(called_from_db(From, IM:Goal, Caller)).

print_call_point(L-A) :-
	print_message(information, acheck(called_from(L, A))).
