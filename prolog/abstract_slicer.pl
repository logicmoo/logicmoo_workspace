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

:- use_module(xlibrary(implementation_module)).
:- use_module(xlibrary(abstract_interpreter)).

:- multifile
    evaluable_goal_hook/2.
:- dynamic
    evaluable_goal_hook/2.

:- meta_predicate abstract_slice(0, +).

evaluable_goal_hook(absolute_file_name(A, _, O), _) :-
    ground(A),
    ground(O).
evaluable_goal_hook(memberchk(E, L), _) :-
    is_list(L),
    nonvar(E).
evaluable_goal_hook(option(O, L), _) :-
    is_list(L),
    nonvar(O).

abstract_slice(M:Call, Mode) :-
    apply_mode(Call, Mode, Spec),
    abstract_interpreter(M:Call, slicer_abstraction(Spec)).

apply_mode(Call, Mode, Spec) :-
    functor(Call, F, A),
    functor(Spec, F, A),
    apply_mode_arg(1, Call, Mode, Spec).

apply_mode_arg(N0, Call, Mode, Spec) :-
    arg(N0, Call, Arg), !,
    arg(N0, Mode, MSp),
    arg(N0, Spec, ASp),
    ( MSp = -
    ->ASp = Arg
    ; true
    ),
    succ(N0, N),
    apply_mode_arg(N, Call, Mode, Spec).
apply_mode_arg(_, _, _, _).

slicer_abstraction(_, Goal, M, M:true, _, _) -->
    { implementation_module(M:Goal, IM),
      evaluable_goal_hook(Goal, IM), !,
      call(M:Goal)
    }.
slicer_abstraction(Spec, Goal, M, CMBody, _, _) -->
    { predicate_property(M:Goal, interpreted), !,
      ( terms_share(Spec, Goal)
      ->match_head_body(Goal, M, CMBody)
      ;	% check if the body trivially fails:
	once(match_head_body(Goal, M, _Body)),
	CMBody = M:true
      )
    }.
slicer_abstraction(_, Goal, M, _, _, _) -->
    { \+ predicate_property(M:Goal, defined),
      !,
      fail
    }.
slicer_abstraction(_, _, M, M:true, _, _) --> bottom.
