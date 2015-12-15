/*  Part of Extended libraries for Prolog

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

:- module(abstract_slicer, [abstract_slice/2]).

:- use_module(library(abstract_interpreter)).

:- meta_predicate abstract_slice(0, +).

abstract_slice(M:Call, Mode) :-
    abstract_interpreter(M:Call, slicer_abstraction(Call, Mode)).

slicer_abstraction(Call, Mode, Goal, M, Body, _, _) -->
    {predicate_property(M:Goal, interpreted)}, !,
    { match_head_body(Goal, M, Body0 )
    *->true
    ; throw(fail_branch)
    },
    { call_mode(Call, Mode)
    ->Body = true
    ; Body = Body0
    }.
slicer_abstraction(_, _, fail, _, true, _, _) --> !, {throw(fail_branch)}.
slicer_abstraction(_, _, true, _, true, _, _) --> !, [].
slicer_abstraction(_, _, !,    _, true, _, _) --> !, [].
slicer_abstraction(_, _, _,    _, true, _, _) --> bottom.

call_mode(Call, Mode) :-
    forall(arg(N, Mode, Spec),
	   ( arg(N, Call, Arg),
	     mode_arg(Spec, Arg)
	   )).

mode_arg(?,   _  ).
mode_arg(*,   _  ).
mode_arg(-,   Arg) :- ground(Arg).
mode_arg(+,   Arg) :- var(Arg).
mode_arg(in,  Arg) :- nonvar(Arg).
mode_arg(out, Arg) :- \+ ground(Arg).
