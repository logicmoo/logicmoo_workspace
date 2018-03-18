/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(nitrace, [nitrace_file/3,
                    nitrace/1,
                    nitrace/3]).

:- use_module(library(ontrace)).
:- use_module(library(prolog_clause), []).

:- meta_predicate nitrace_file(0,+,+).
nitrace_file(Goal, Alias, OptL) :-
    absolute_file_name(Alias, File),
    setup_call_cleanup(
        open(File, write, Stream),
        nitrace(Goal, Stream, OptL),
        close(Stream)).

:- meta_predicate nitrace(0,+,+).
nitrace(Goal, Stream, OptL) :-
    ontrace(Goal, nitrace_port(Stream), OptL).

:- meta_predicate nitrace(0).
nitrace(Goal) :-
    nitrace(Goal, user_output, []).

frame_pi(Frame, PI) :-
    prolog_frame_attribute(Frame, predicate_indicator, PI).

nitrace_port(Stream, Port, Frame, PC, ParentL, SubLoc, continue) :-
    ( maplist(frame_pi, ParentL, CS),
      print_message(stream(Stream, SubLoc), frame(Frame, Port, PC, CS)),
      fail
    ; true
    ).

:- multifile
    user:message_property/2,
    prolog:message//1.

user:message_property(stream(Stream, _), stream(Stream)) :- !.
user:message_property(stream(_, Loc), prefix(F-A)) :- !,
    '$messages':swi_location(Loc, [F1-A], []),
    atomic_list_concat(['~N', F1, '\t'], F).

prolog:message(frame(Frame, redo(Redo), PC, CS)) --> !,
    '$messages':translate_message(frame(Frame, redo, PC, CS)),
    [' - redo(~w)'-[Redo]].
prolog:message(frame(Frame, exitcl, PC, CS)) --> !,
    '$messages':translate_message(frame(Frame, exit, PC, CS)),
    [' - clause'].
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
