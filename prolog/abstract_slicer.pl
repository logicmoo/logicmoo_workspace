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

:- module(abstract_slicer, [abstract_slice/3,
			    slicer_abstraction/9]).

:- use_module(library(abstract_interpreter)).

:- meta_predicate abstract_slice(0, +, ?).

abstract_slice(M:Call, Mode, OptL) :-
    apply_mode(Call, Mode, Spec),
    option(eval_scope(Scope), OptL, body),
    abstract_interpreter(M:Call, slicer_abstraction(Spec, Scope), OptL).

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

slicer_abstraction(Spec, Scope, Goal, M, Body,
		   state(_,   EvalL, OnErr, CallL, Data),
		   state(Loc, EvalL, OnErr, CallL, Data)) -->
    {predicate_property(M:Goal, interpreted)}, !,
    { terms_share(Spec, Goal) % BUG: the sharing should be done wrt all the
                              % body, and not only the current literal --EMM
    ->match_head_body(Goal, M, Body1, Loc),
      ( Scope = body
      ->Body = Body1
      ;	terms_share(Spec, Goal)
      ->Body = Body1
      ; Body = M:true
      )
    ; % check if the body trivially fails:
      ( Scope = body
      ->once(match_head_body(Goal, M, _Body, Loc))
      ; true
      ),
      Body = M:true
    },
    ( {Scope = head}
    ->bottom			% Kludge to avoid cut remove solutions
    ; []
    ).
slicer_abstraction(_, _, Goal, M, M:true, S, S) -->
    { S = state(Loc, _, OnError, _, _),
      call(OnError, error(existence_error(evaluation_rule, M:Goal), Loc))
    },
    bottom.
