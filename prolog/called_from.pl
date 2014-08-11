:- module(called_from, [called_from/1,
			called_from/2,
			called_from/4,
			collect_called_from/3,
			collect_called_from/4,
			used_predicates/3]).

:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(implementation_module)).
:- use_module(library(location_utils)).
:- use_module(library(record_locations)).

:- multifile
    prolog:message//1.

:- dynamic called_from_db/4.

prolog:message(acheck(called_from(MsgLoc, Args))) -->
	MsgLoc,
	['~w called from ~w'-Args].

called_from(Ref) :-
	called_from(Ref, _).

called_from(Ref, Caller) :-
    called_from(Ref, Caller, [], Sorted),
    maplist(print_call_point, Sorted),
    cleanup_locations(_, _, dynamic(_, _), _),
    retractall(called_from_db(_, _, _, _)).

called_from(Ref0, Caller, OptionL, Pairs) :-
    normalize_head(Ref0, Ref),
    collect_called_from(Ref, Caller, OptionL, Pairs).

collect_called_from(Ref, Caller, OptionL, Sorted) :-
    collect_called_from(Ref, Caller, OptionL),
    findall(L-[M:PI, CPI], ( Ref = M:_,
			   current_called_from(Ref, F, PI, C),
			   normalize_pi(C, CPI),
			   from_location(F, L)
			 ), Pairs),
    keysort(Pairs, Sorted).

collect_called_from(Ref, Caller, OptionL0) :-
    cleanup_locations(_, _, dynamic(_, _), _),
    retractall(called_from_db(_, _, _, _)),
    merge_options([infer_meta_predicates(false),
		   autoload(false),
		   evaluate(false),
		   trace_reference(Ref),
		   module_class([user, system, library]),
		   on_trace(collect_call_point(Caller))],
		  OptionL0, OptionL),
    prolog_walk_code(OptionL).

current_called_from(M:H, From, PI, Caller) :-
    ( called_from_db(H, M, From, Caller)
    ; declaration_location(H, M, dynamic(Type, Caller), From),
      memberchk(Type, [retract, query])
    ),
    functor(H, F, A),
    PI = F/A.

:- public collect_call_point/4.
collect_call_point(Caller, MGoal, Caller, From) :-
    implementation_module(MGoal, IM),
    record_location_dynamic(MGoal, From),
    MGoal = _M:Goal,
    assertz(called_from_db(Goal, IM, From, Caller)).

print_call_point(L-A) :-
    print_message(information, acheck(called_from(L, A))).

% used_predicates(+Module, +Context, -PIL) is det
%
% Unifies PIL with a list of predicates implemented in the module Module,
% actually being used in the context Context.  Note that this would be different
% than the imported predicates.
%
used_predicates(Module, Context, PIL) :-
    collect_called_from(Module:_, _, [module(Context), source(false)]),
    findall(PI, current_called_from(Module:_, _, PI, _), PIU),
    sort(PIU, PIL).
