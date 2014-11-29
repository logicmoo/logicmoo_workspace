:- module(nitrace, [nitrace_file/3,
		    nitrace/3]).

:- use_module(library(maplist_dcg)).
:- use_module(library(ontrace)).


:- meta_predicate nitrace_file(0, +, +).

nitrace_file(Goal, Path, Alias) :-
    absolute_file_name(Alias, File),
    setup_call_cleanup(
	open(File, write, Stream),
	nitrace(Goal, Path, Stream),
	close(Stream)).

:- meta_predicate nitrace(0, +, +).

nitrace(Goal, Path, Stream) :-
    ontrace(Goal, nitrace_port(Path, Stream)).

nitrace_port(Path, Stream, Frame, Port, PC, CS, Cl) :-
    ( nonvar(Cl),
      clause_property(Cl, file(AFile)),
      clause_property(Cl, line_count(Line))
    ->directory_file_path(Path, File, AFile), % trace only files in Path
      StreamLoc = stream(Stream, File, Line, CS)
    ; StreamLoc = stream(Stream, CS)
    ),
    print_message(StreamLoc, frame(Frame, Port, PC)).

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
