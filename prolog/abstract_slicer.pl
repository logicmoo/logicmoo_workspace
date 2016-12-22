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
    apply_mode(Call, Mode, Spec, RevS),
    option(eval_scope(Scope), OptL, body),
    abstract_interpreter(M:Call, slicer_abstraction(Spec, RevS, Scope), OptL).

apply_mode(Call, Mode, Spec, RevS) :-
    functor(Call, F, A),
    functor(Spec, F, A),
    functor(RevS, F, A),
    apply_mode_arg(1, Call, Mode, Spec, RevS).

apply_mode_arg(N0, Call, Mode, Spec, RevS) :-
    arg(N0, Call, Arg), !,
    arg(N0, Mode, MSp),
    arg(N0, Spec, ASp),
    arg(N0, RevS, ARs),
    ( MSp = -
    ->ASp = Arg,
      ARs = -
    ; ASp = +,
      ARs = Arg
    ),
    succ(N0, N),
    apply_mode_arg(N, Call, Mode, Spec, RevS).
apply_mode_arg(_, _, _, _, _).

chain_of_dependencies(Spec, RevS, Goal, ContL) :-
    \+ ground(Goal),
    ( terms_share(Spec, RevS, Goal)
    ->true
    ; select(Cont, ContL, ContL2),
      terms_share(Cont, RevS, Goal),
      chain_of_dependencies(Spec, RevS, Cont, ContL2)
    ), !.

terms_share(A, R, B) :-
    term_variables(A, VarsA),
    VarsA \= [], % Optimization
    term_variables(B, VarsB),
    term_variables(R, VarsR),
    ( member(VA, VarsA),
      member(VB, VarsB),
      VA==VB,
      \+ ( member(VR, VarsR),
	   VA == VR
	 )
    ), !.

slicer_abstraction(Spec, RevS, Scope, Goal, M, Body,
		   state(_, EvalL, OnErr, CallL, Data, Cont),
		   state(Loc, EvalL, OnErr, CallL, Data, Cont)) -->
    {predicate_property(M:Goal, interpreted)}, !,
    { \+ ground(Spec),
      chain_of_dependencies(Spec, RevS, Goal, Cont)
    ->match_head_body(Goal, M, Body1, Loc),
      ( Scope = body
      ->Body = Body1
      ;	terms_share(Spec, RevS, Goal)
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
    { S = state(Loc, _, OnError, _, _, _),
      call(OnError, error(existence_error(evaluation_rule, M:Goal), Loc))
    },
    bottom.
