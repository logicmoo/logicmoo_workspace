:- module(rtchecks_trace, [trace_rtc/1]).

:- use_module(library(listing)).
:- use_module(library(maplist_dcg)).
:- use_module(library(ontrace)).
:- use_module(rtchecks(rtchecks_rt)).
:- use_module(rtchecks(rtchecks_tr)).
:- use_module(rtchecks(rtchecks_eval)).

:- meta_predicate trace_rtc(0).
trace_rtc(Goal) :-
    State=state(_, _, _), % Allow destructive assignment
    call_inoutex(Goal,
		 setup_trace(State),
		 ontrace:cleanup_trace(State)).

setup_trace(State) :-
    asserta((user:prolog_trace_interception(Port, Frame, _, Action)
	    :- rtcheck_port(Port, Frame, Action)),
	    Ref),
    maplist_dcg(ontrace:port_mask, [unify], 0, Mask),
    '$visible'(Visible, Mask),
    '$leash'(Leash, Mask),
    nb_setarg(1, State, Visible),
    nb_setarg(2, State, Leash),
    nb_setarg(3, State, Ref),
    trace.

black_list_module(assrt_lib).
black_list_module(rtchecks_tr).
black_list_module(rtchecks_rt).
black_list_module(rtchecks_eval).
black_list_module(rtchecks_trace).
black_list_module(rtchecks_utils).
black_list_module(rtchecks_basic).
black_list_module(rtchecks_send).
black_list_module('$expand').
black_list_module(complete_dict).
black_list_module(native_props).
black_list_module(basic_props).
black_list_module(apply_dict).
black_list_module(exceptions).
black_list_module(intercept).
black_list_module(compact_list).
black_list_module(exceptions_db).
black_list_module(hiordlib).
black_list_module(system).
black_list_module(ontrace).
black_list_module(maplist_dcg).
black_list_module(expansion_module).

skip_predicate(rtchecks_utils:handle_rtcheck(_)).

rtcheck_port(unify, Frame, _) :-
    prolog_frame_attribute(Frame, context_module, CM),
    \+ black_list_module(CM),
    prolog_frame_attribute(Frame, clause, Clause),
    PI=M:F/A,
    '$break_pc'(Clause, PC, _NextPC1),
    '$fetch_vm'(Clause, PC, _NextPC2, TInstr),
    ( memberchk(TInstr, [i_call(PI), i_depart(PI)]),
      \+ black_list_module(M),
      functor(Goal, F, A),
      current_assertion(Goal, M, rtcheck, _)
    ->'$break_at'(Clause, PC, true),
      fail
    ).
rtcheck_port(_, Frame, Action) :-
    ( prolog_frame_attribute(Frame, goal, Goal),
      \+ skip_predicate(Goal)
    ->Action = continue
    ; Action = skip
    ).

clause_location(Clause, PC, Loc) :-
    '$clause_term_position'(Clause, PC, List),
    ontrace:clause_subloc(Clause, List, Loc).

:- multifile
    prolog:message_location//1.

prolog:message_location(clause_pc(Clause, PC)) -->
    {clause_location(Clause, PC, Loc)},
    prolog:message_location(Loc).

prolog:break_hook(Clause, PC, FR, _, call(Goal), call(RTChecks)) :-
    prolog_frame_attribute(FR, context_module, M),
    generate_rtchecks(clause_pc(Clause, PC), M:Goal, RTChecks),
    '$break_at'(Clause, PC, false).
