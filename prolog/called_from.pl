:- module(called_from, [called_from/1, called_from/2]).

:- use_module(tools(tools_common)).
:- use_module(tools(location_utils)).
:- use_module(tools(record_locations_db)).

:- multifile
	prolog:message//1.

:- dynamic called_from_db/2.

prolog:message(acheck(called_from(MsgLoc, Args))) -->
	MsgLoc,
	['~w called from ~w'-Args].

called_from(Ref) :-
	called_from(Ref, _).

called_from(Ref, Caller) :-
	called_from_meta(Ref, collect_call_point(Caller)).

:- meta_predicate called_from_meta(?,3).
called_from_meta(Ref0, OnTrace) :-
	normalize_head(Ref0, Ref),
	current_prolog_flag(verbose, Verbose),
	set_prolog_flag(verbose, silent),
	call_cleanup(prolog_walk_code([trace_reference(Ref),
				       module_class([user,system,library]),
				       infer_meta_predicates(false),
				       evaluate(false),
				       on_trace(OnTrace)]),
		     set_prolog_flag(verbose, Verbose)),
	findall(L-Args, ( ( retract(called_from_db(From, Args))
			  ; Ref = M:H,
			    nonvar(H),
			    functor(H, F, A),
			    declaration_location(M:F/A, dynamic(Type, Call), From),
			    Type \= def,
			    Args = [M:F/A, Call]
			  ),
			  from_location(From, L)
			), Pairs),
	keysort(Pairs, Sorted),
	maplist(print_call_point, Sorted),
	cleanup_locations(_, dynamic(_, _), _),
	fail.
called_from_meta(_, _).

collect_call_point(Caller, MGoal, Caller, From) :-
    implementation_module(MGoal, IM),
    record_location_dynamic(MGoal, From),
    MGoal = _M:Goal,
    functor(Goal, F, A),
    normalize_pi(Caller, PI),
    Args = [IM:F/A, PI],
    assertz(called_from_db(From, Args)).

print_call_point(L-A) :-
	print_message(information, acheck(called_from(L, A))).
