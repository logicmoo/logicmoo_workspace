/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- module(abstract_slicer, [abstract_slice/3,
                            slicer_abstraction/10]).

:- use_module(library(abstract_interpreter)).

:- meta_predicate abstract_slice(0, +, ?).

abstract_slice(M:Call, Mode, OptL) :-
    apply_mode(Call, Mode, Spec, RevS),
    term_variables(RevS, VarsR),
    option(eval_scope(Scope), OptL, body),
    abstract_interpreter(M:Call, slicer_abstraction(Spec, VarsR, Scope), OptL).

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

chain_of_dependencies(Spec, VarsR, Goal, ContL) :-
    \+ ground(Goal),
    ( terms_share(Spec, VarsR, Goal)
    ->true
    ; select(Cont, ContL, ContL2),
      terms_share(Cont, VarsR, Goal),
      chain_of_dependencies(Spec, VarsR, Cont, ContL2)
    ), !.

terms_share(A, VarsR, B) :-
    term_variables(A, VarsA),
    VarsA \= [], % Optimization
    term_variables(B, VarsB),
    ( member(VA, VarsA),
      member(VB, VarsB),
      VA==VB,
      \+ ( member(VR, VarsR),
           VA == VR
         )
    ), !.

slicer_abstraction(Spec, VarsR, Scope, Goal, M, Body,
                   state(_, EvalL, OnErr, CallL, Data, Cont),
                   state(Loc, EvalL, OnErr, CallL, Data, Cont)) -->
    {predicate_property(M:Goal, interpreted)}, !,
    { \+ ground(Spec),
      chain_of_dependencies(Spec, VarsR, Goal, Cont)
    ->match_head_body(M:Goal, Body1, Loc),
      ( Scope = body
      ->Body = Body1
      ; terms_share(Spec, VarsR, Goal)
      ->Body = Body1
      ; Body = M:true
      )
    ; % check if the body trivially fails:
      ( Scope = body
      ->once(match_head_body(M:Goal, _Body, Loc))
      ; true
      ),
      Body = M:true
    },
    ( {Scope = head}
    ->bottom                    % Kludge to avoid cut remove solutions
    ; []
    ).
slicer_abstraction(_, _, _, Goal, M, M:true, S, S) -->
    { S = state(Loc, _, OnError, _, _, _),
      call(OnError, error(existence_error(evaluation_rule, M:Goal), Loc))
    },
    bottom.
