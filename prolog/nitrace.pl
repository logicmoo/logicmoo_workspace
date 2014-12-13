:- module(nitrace, [nitrace_file/2,
		    nitrace/2]).

:- use_module(library(maplist_dcg)).
:- use_module(library(ontrace)).
:- use_module(library(prolog_clause), []).

:- meta_predicate nitrace_file(0,+).
nitrace_file(Goal, Alias) :-
    absolute_file_name(Alias, File),
    setup_call_cleanup(
	open(File, write, Stream),
	nitrace(Goal, Stream),
	close(Stream)).

:- meta_predicate nitrace(0,+).
nitrace(Goal, Stream) :-
    ontrace(Goal, nitrace_port(Stream)).

frame_pi(Frame, PI) :-
    prolog_frame_attribute(Frame, predicate_indicator, PI).

nitrace_port(Stream, Port, Frame, PC, ParentL, SubLoc) :-
    maplist(frame_pi, ParentL, CS),
    print_message(stream(Stream, SubLoc), frame(Frame, Port, PC, CS)).

:- multifile
    user:message_property/2,
    prolog:message_location//1,
    prolog:message//1.

user:message_property(stream(Stream, _), stream(Stream)) :- !.
user:message_property(stream(_, Loc), prefix(F-A)) :- !,
    prolog:message_location(Loc, [F0-A], []),
    atomic_list_concat(['~N', F0, '\t'], F).

prolog:message(frame(Frame, redo(Redo), PC, CS)) --> !,
    '$messages':translate_message(frame(Frame, redo, PC, CS)),
    [' - redo(~w)'-[Redo]].
prolog:message(frame(Frame, exception(Ex), PC, CS)) --> !,
    '$messages':translate_message(frame(Frame, exception, PC, CS)),
    [nl],
    '$messages':translate_message(Ex).
prolog:message(frame(Frame, Port, PC, CS)) -->
    '$messages':translate_message(frame(Frame, Port, PC)),
    ( {CS = []}
    ->[]
    ; [' (caller: ~q)'-[CS]]
    ).
