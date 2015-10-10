/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(nitrace, [nitrace_file/3,
		    nitrace/3]).

:- use_module(xtools(ontrace)).
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

frame_pi(Frame, PI) :-
    prolog_frame_attribute(Frame, predicate_indicator, PI).

nitrace_port(Stream, Port, Frame, PC, ParentL, SubLoc, continue) :-
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
