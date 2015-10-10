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

:- module(called_by_body, [called_by_body/4]).

:- use_module(xlibrary(extend_args)).
:- use_module(xlibrary(implementation_module)).

called_by_body(Body, CM, Body, CM) :- var(Body), !, fail.
called_by_body(CM:Body, _, H, M) :- called_by_body(Body, CM, H, M).
called_by_body((A,B),  CM, H, M) :- !,
    ( called_by_body(A, CM, H, M)
    ; called_by_body(B, CM, H, M)
    ).
called_by_body((A;B),  CM, H, M) :- !,
    ( called_by_body(A, CM, H, M)
    ; called_by_body(B, CM, H, M)
    ).
called_by_body(Goal, CM, H, M) :-
    predicate_property(CM:Goal, meta_predicate(Spec)),
    called_by_args(Goal, Spec, CM, H, M).
called_by_body(Goal, CM, Goal, M) :-
    implementation_module(CM:Goal, M).

called_by_args(Goal, Spec, CM, H, M) :-
    arg(N, Goal, Arg),
    arg(N, Spec, SA),
    called_by_arg(SA, Arg, CM, H, M).

called_by_arg(0, Goal, CM, H, M) :- !, called_by_body(Goal, CM, H, M).
called_by_arg(^, Goal, CM, H, M) :- !, called_by_body(Goal, CM, H, M).
called_by_arg(N, Goal, CM, H, M) :-
    integer(N),
    length(Extra, N),
    extend_args(Goal, Extra, Goal1),
    called_by_body(Goal1, CM, H, M).
