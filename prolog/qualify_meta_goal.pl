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

:- module(qualify_meta_goal, [qualify_meta_goal/2,
			      qualify_meta_goal/3,
			      qualify_meta_goal/4]).

:- use_module(library(check), []). % for add_module/3

qualify_meta_goal(Goal0, M, CM, Goal) :-
    predicate_property(M:Goal0, meta_predicate(Meta)), !,
    qualify_meta_goal(CM:Goal0, Meta, Goal).
qualify_meta_goal(Goal, _, _, Goal).

:- meta_predicate qualify_meta_goal(0, -).
qualify_meta_goal(Goal0, Goal) :-
    predicate_property(Goal0, meta_predicate(Meta)), !,
    qualify_meta_goal(Goal0, Meta, Goal).
qualify_meta_goal(_:Goal, Goal).

qualify_meta_goal(M:Goal0, Meta, Goal) :-
    functor(Goal0, F, N),
    functor(Goal, F, N),
    meta_goal(1, M, Meta, Goal0, Goal).

module_qualified(:) :- !.
module_qualified(N) :- integer(N), N >= 0.

meta_goal(N, M, Meta, Goal0, Goal) :-
    arg(N, Meta,  ArgM),
    !,
    arg(N, Goal0, Arg0),
    arg(N, Goal,  Arg),
    succ(N, N1),
    ( module_qualified(ArgM) ->
      check:add_module(Arg0, M, Arg)
    ; Arg = Arg0
    ),
    meta_goal(N1, M, Meta, Goal0, Goal).
meta_goal(_, _, _, _, _).
