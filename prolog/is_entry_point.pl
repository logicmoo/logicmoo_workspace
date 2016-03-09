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

:- module(is_entry_point, [is_entry_point/2]).

is_entry_property(exported).
is_entry_property((public)).
is_entry_property(imported_from(_)).

:- dynamic http_dispatch:handler/4.
:- multifile http_dispatch:handler/4.

:- multifile
    is_entry_point_hook/2,
    not_entry_point_hook/2.

is_entry_point_hook(term_expansion(_, _), _).
is_entry_point_hook(goal_expansion(_, _), _).
is_entry_point_hook(term_expansion(_, _, _, _), _).
is_entry_point_hook(goal_expansion(_, _, _, _), _).
is_entry_point_hook(thread_message_hook(_, _, _), user).
is_entry_point_hook(prolog_exception_hook(_, _, _, _), user).
is_entry_point_hook(prolog_load_file(_, _), user).
is_entry_point_hook(message_hook(_, _, _), user).
is_entry_point_hook(prolog_trace_interception(_, _, _, _), user).
is_entry_point_hook(_, prolog).
is_entry_point_hook(doc_db(_, _, _, _), assrt_lib).

% not_entry_point_hook(head_prop_asr(_, _, _, _, _, _, _, _), assrt_lib).

is_entry_point(H, M) :- is_entry_point_hook(H, M), !.
% is_entry_point(H, M) :- not_entry_point_hook(H, M), !, fail.
is_entry_point(H, M) :-
    functor(H, Name, A),
    A>0,
    succ(A2, A),
    functor(H2, Name, A2),
    http_dispatch:handler(_, M:H2, _, _), !.
is_entry_point(H, M) :-
    is_entry_property(Prop),
    predicate_property(M:H, Prop), !.
