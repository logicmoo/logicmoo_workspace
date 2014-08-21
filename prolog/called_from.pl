:- module(called_from, [called_from/1,
			called_from/2,
			called_from/5,
			collect_called_from/5,
			collect_called_from/6,
			current_called_from/5,
			current_used_from/6,
			used_predicates/2,
			used_predicates/3
		       ]).

:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(implementation_module)).
:- use_module(library(location_utils)).
:- use_module(library(record_locations)).

:- multifile
    prolog:message//1.

:- dynamic called_from_db/5.

prolog:message(acheck(called_from(MsgLoc, Args))) -->
	MsgLoc,
	['~w called from ~w'-Args].

called_from(Ref) :-
	called_from(Ref, _).

called_from(Ref, Caller) :-
    called_from(Ref, _CM, Caller, [], Sorted),
    maplist(print_call_point, Sorted),
    cleanup_locations(_, _, dynamic(_, _, _), _),
    retractall(called_from_db(_, _, _, _, _)).

called_from(Ref0, CM, Caller, OptionL, Pairs) :-
    normalize_head(Ref0, M:H),
    collect_called_from(H, M, CM, Caller, OptionL, Pairs).

collect_called_from(H, M, CM, Caller, OptionL, Sorted) :-
    collect_called_from(H, M, CM, Caller, OptionL),
    findall(Loc-[M:F/A, CPI], ( current_called_from(H, M, CM, From, C),
				functor(H, F, A),
				normalize_pi(C, CPI),
				from_location(From, Loc)
			      ), Pairs),
    keysort(Pairs, Sorted).

collect_called_from(Ref, M, CM, Caller, OptionL0) :-
    cleanup_locations(_, _, dynamic(_, _, _), _),
    retractall(called_from_db(_, _, _, _, _)),
    merge_options([infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(_:Ref),
		   module_class([user, system, library]),
		   on_trace(collect_call_point(M, CM, Caller))],
		  OptionL0, OptionL),
    prolog_walk_code(OptionL).

current_called_from(H, M, CM, From, Caller) :-
    current_used_from([retract, query], H, M, CM, From, Caller).

current_used_from(DynTypes, H, M, CM, From, Caller) :-
    ( called_from_db(H, M, CM, From, Caller)
    ; declaration_location(H, M, dynamic(Type, CM, Caller), From),
      memberchk(Type, DynTypes)
    ; declaration_location(H, CM, goal, From),
      implementation_module(CM:H, M)
    ).

:- public collect_call_point/6.
collect_call_point(IM, M, Caller, MGoal, Caller, From) :-
    record_location_dynamic(MGoal, IM, From),
    MGoal = M:Goal,
    implementation_module(MGoal, IM),
    assertz(called_from_db(Goal, IM, M, From, Caller)).

print_call_point(L-A) :-
    print_message(information, acheck(called_from(L, A))).

% used_predicates(+Module, +Context, -PIL) is det
%
% Unifies PIL with a list of predicates implemented in the module Module,
% actually being used in the context Context.  Note that this would be different
% than the imported predicates.
%
used_predicates(Module, Context, PIL) :-
    collect_called_from(_, Module, Context, _, [source(false)]),
    findall(F/A,
	    ( current_called_from(H, Module, Context, _, _),
	      functor(H, F, A)
	    ), PIU),
    sort(PIU, PIL).

used_predicates(Module, Groups) :-
    collect_called_from(_, Module, _, _, [source(false)]),
    findall(Context-(F/A),
	    ( current_called_from(H, Module, Context, _, _),
	      functor(H, F, A)
	    ), Pairs),
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Groups).
