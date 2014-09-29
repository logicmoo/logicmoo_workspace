:- module(nitrace, [nitrace_file/3,
		    nitrace/3]).

:- meta_predicate nitrace_file(0, +, +).

nitrace_file(Goal, Path, Alias) :-
    absolute_file_name(Alias, File),
    setup_call_cleanup(
	open(File, write, Stream),
	nitrace(Goal, Path, Stream),
	close(Stream)).

:- meta_predicate nitrace(0, +, +).

nitrace(Goal, Path, Stream) :-
    setup_call_cleanup(
	setup_trace(State, Path, Stream),
	Goal,
	cleanup_trace(State)).

setup_trace(state(Visible, Leash, Ref), Path, Stream) :-
    asserta((user:prolog_trace_interception(Port, Frame, PC, continue)
	    :- trace_port(Port, Frame, PC, Path, Stream)), Ref),
    prolog_cover:port_mask([call,
			    exit,
			    fail,
			    redo,
			    unify,
			    exception
			   ], Mask),
    '$visible'(Visible, Mask),
    '$leash'(Leash, Mask),
    trace.

:- multifile user:message_property/2.

user:message_property(stream(Stream, _),       stream(Stream)) :- !.
user:message_property(stream(Stream, _, _, _), stream(Stream)) :- !.
user:message_property(stream(_, []), prefix('~N')) :- !.
user:message_property(stream(_, CS), prefix('~N ~w ->'-[CS])) :- !.
user:message_property(stream(_, File, Line, []), prefix('~N~w:~d:'-[File, Line])) :- !.
user:message_property(stream(_, File, Line, CS),
		      prefix('~N~w:~d: ~w ->'-[File, Line, CS])) :- !.

:- multifile skip_trace/1.
:- dynamic skip_trace/1.

skip_trace(M:_) :-
    \+ module_property(M, class(user)). % trace only user predicates

:- public trace_port/5.

trace_port(Port, Frame, PC, Path, Stream) :-
    % Module qualification to skip local predicates:
    prolog_frame_attribute(Frame, predicate_indicator, M:PI),
    \+ skip_trace(M:PI), !,
    trace_port_(Port, Frame, PC, Path, Stream).
trace_port(_, _, _, _, _).

trace_port_(redo(_), Frame, PC, Path, Stream) :- !,
    trace_port(redo, Frame, PC, Path, Stream).
trace_port_(exception(Ex), Frame, PC, Path, Stream) :- !,
    trace_port(exception, Frame, PC, Path, Stream),
    print_message(stream(Stream, []), Ex).
trace_port_(Port, Frame, PC0, Path, Stream) :-
    ( find_frame_clause(Frame, PC0, PC, CS, Cl)
    ->clause_stream_loc(Cl, Path, Stream, CS, StreamLoc)
    ; StreamLoc = stream(Stream, [])
    ), !,
    print_message(StreamLoc, frame(Frame, Port, PC)).
trace_port_(_, _, _, _, _).

find_frame_clause(Frame, PC, PC, [], Cl) :-
    prolog_frame_attribute(Frame, clause, Cl), !.
find_frame_clause(Frame, _, PC, CS, Cl) :-
    find_frame_clause_rec(Frame, PC, CS, Cl).

find_frame_clause_rec(Frame, PC, [PI|CS], Cl) :-
    prolog_frame_attribute(Frame, parent, Parent),
    ( prolog_frame_attribute(Frame, pc, PC)
    ->prolog_frame_attribute(Parent, clause, Cl),
      prolog_frame_attribute(Parent, predicate_indicator, PI),
      CS = []
    ; prolog_frame_attribute(Parent, predicate_indicator, PI),
      find_frame_clause_rec(Parent, PC, CS, Cl)
    ).

clause_stream_loc(Cl, Path, Stream, CS, StreamLoc) :-
    ( clause_property(Cl, file(AFile)),
      clause_property(Cl, line_count(Line))
    ->directory_file_path(Path, File, AFile), % trace only files in Path
      StreamLoc = stream(Stream, File, Line, CS)
    ; StreamLoc = stream(Stream, CS)
    ).

cleanup_trace(state(Visible, Leash, Ref)) :-
    nodebug,
    '$visible'(_, Visible),
    '$leash'(_, Leash),
    erase(Ref),
    !.
cleanup_trace(_) :-
    print_message(error, format('Failed when saving tracer data', [])),
    fail.
