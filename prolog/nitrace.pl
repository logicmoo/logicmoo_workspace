:- module(nitrace, [nitrace_file/3,
		    nitrace/3]).

:- use_module(library(maplist_dcg)).

:- meta_predicate nitrace_file(0, +, +).

nitrace_file(Goal, Path, Alias) :-
    absolute_file_name(Alias, File),
    setup_call_cleanup(
	open(File, write, Stream),
	nitrace(Goal, Path, Stream),
	close(Stream)).

:- meta_predicate nitrace(0, +, +).

nitrace(Goal, Path, Stream) :-
    State=state(_, _, _),	% Allow destructive assignment
    call_inout(Goal,
	setup_trace(State, Path, Stream),
	cleanup_trace(State)).

:- meta_predicate call_inout(0, 0, 0).
call_inout(Goal, OnInp, OnOut) :-
    (OnInp;OnOut,fail),
    prolog_current_choice(C0),
    catch(Goal, E, (OnOut, throw(E))),
    prolog_current_choice(C1),
    (OnOut;OnInp,fail),
    (C0==C1 -> ! ;true).

%% setup_trace(!State, +Path, +Stream) is det.
setup_trace(State, Path, Stream) :-
    asserta((user:prolog_trace_interception(Port, Frame, PC, continue)
	    :- trace_port(Port, Frame, PC, nitrace_port(Path, Stream))), Ref),
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

port_mask(Port, Mask0, Mask) :- '$syspreds':port_name(Port, Bit),
    Mask is Mask0\/Bit.

:- multifile
    user:message_property/2,
    prolog:message//1.

user:message_property(stream(Stream, _),       stream(Stream)) :- !.
user:message_property(stream(Stream, _, _, _), stream(Stream)) :- !.
user:message_property(stream(_, []), prefix('~N')) :- !.
user:message_property(stream(_, CS), prefix('~N ~w ->'-[CS])) :- !.
user:message_property(stream(_, File, Line, []), prefix('~N~w:~d:'-[File, Line])) :- !.
user:message_property(stream(_, File, Line, CS),
		      prefix('~N~w:~d: ~w ->'-[File, Line, CS])) :- !.

prolog:message(frame(Frame, redo(Redo), PC)) -->
    '$messages':translate_message(frame(Frame, redo, PC)),
    [' - ~w'-[Redo]].
prolog:message(frame(Frame, exception(Ex), PC)) -->
    '$messages':translate_message(frame(Frame, exception, PC)),
    [nl],
    '$messages':translate_message(Ex).

:- multifile skip_trace/1.
:- dynamic skip_trace/1.

skip_trace(M:_) :-
    \+ module_property(M, class(user)). % trace only user predicates
skip_trace(nitrace:_).

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

:- public nitrace_port/7.

nitrace_port(Path, Stream, Frame, Port, PC, CS, Cl) :-
    ( nonvar(Cl),
      clause_property(Cl, file(AFile)),
      clause_property(Cl, line_count(Line))
    ->directory_file_path(Path, File, AFile), % trace only files in Path
      StreamLoc = stream(Stream, File, Line, CS)
    ; StreamLoc = stream(Stream, CS)
    ),
    print_message(StreamLoc, frame(Frame, Port, PC)).

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
