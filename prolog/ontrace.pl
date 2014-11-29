:- module(ontrace, [ontrace/2]).

:- use_module(library(maplist_dcg)).

:- meta_predicate ontrace(0, 5).

ontrace(Goal, OnTrace) :-
    State=state(_, _, _),	% Allow destructive assignment
    call_inout(Goal,
	setup_trace(State, OnTrace),
	cleanup_trace(State)).

:- meta_predicate call_inout(0, 0, 0).
call_inout(Goal, OnIn, OnOut) :-
    (OnIn;OnOut,fail),
    prolog_current_choice(C0),
    catch(Goal, E, (OnOut, throw(E))),
    prolog_current_choice(C1),
    (OnOut;OnIn,fail),
    (C0==C1 -> ! ;true).

%% setup_trace(!State, :OnTrace) is det.
setup_trace(State, OnTrace) :-
    asserta((user:prolog_trace_interception(Port, Frame, PC, continue)
	    :- trace_port(Port, Frame, PC, OnTrace)), Ref),
    maplist_dcg(port_mask, [call, exit, fail, redo, unify, exception], 0, Mask),
    '$visible'(Visible, Mask),
    '$leash'(Leash, Mask),
    nb_setarg(1, State, Visible),
    nb_setarg(2, State, Leash),
    nb_setarg(3, State, Ref),
    trace.

%% cleanup_state(+state) is det.
cleanup_trace(state(Visible, Leash, Ref)) :-
    nodebug,
    '$visible'(_, Visible),
    '$leash'(_, Leash),
    erase(Ref),
    !.
cleanup_trace(State) :-
    print_message(error, format('Failed when saving tracer data', [State])),
    fail.

:- public trace_port/4.

:- meta_predicate trace_port(+,+,+,5).

trace_port(Port, Frame, PC0, OnTrace) :-
    ( find_frame_clause(Frame, PC0, PC, CS, Cl, M:H)
    ->true
    ; prolog_frame_attribute(Frame, goal, M:H), % M:H to skip local predicates
      CS = []
    ),
    \+ skip_trace(M:H), !,
    call(OnTrace, Frame, Port, PC, CS, Cl).
trace_port(_, _, _, _).

find_frame_clause(Frame, PC, PC, [], Cl, M:H) :-
    prolog_frame_attribute(Frame, clause, Cl), !,
    clause_property(Cl, predicate(M:F/A)),
    functor(H, F, A).
find_frame_clause(Frame, _, PC, CS, Cl, G) :-
    find_frame_clause_rec(Frame, PC, CS, Cl, G).

find_frame_clause_rec(Frame, PC, [PI|CS], Cl, G) :-
    prolog_frame_attribute(Frame, parent, Parent),
    ( prolog_frame_attribute(Frame, pc, PC)
    ->prolog_frame_attribute(Parent, clause, Cl),
      prolog_frame_attribute(Parent, predicate_indicator, PI),
      prolog_frame_attribute(Frame, goal, G),
      CS = []
    ; prolog_frame_attribute(Parent, predicate_indicator, PI),
      find_frame_clause_rec(Parent, PC, CS, Cl, G)
    ).
