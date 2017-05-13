/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(unfold_calls,
          [unfold_calls/4]).

:- use_module(library(implementation_module)).
:- use_module(library(qualify_meta_goal)).

:- multifile
    unfold_call_hook/4.

:- meta_predicate unfold_calls(+, +, 2, -).
unfold_calls(Goal, CM, IsUnfold, Calls) :-
    implementation_module(CM:Goal, M),
    findall(Call, ( unfold_call(Goal, CM, IsUnfold, [], Call),
                    \+ M:Goal =@= Call
                  ), Calls).

unfold_call(Goal, _, _, _, _) :- var(Goal), !, fail.
unfold_call(true, _, _, _, _) :- !, fail.
unfold_call(call(Goal), CM, IsUnfold, NonUnfoldL, Call) :- !,
    unfold_call(Goal, CM, IsUnfold, NonUnfoldL, Call).
unfold_call(\+ Goal, CM, IsUnfold, NonUnfoldL, Call) :- !,
    unfold_call(Goal, CM, IsUnfold, NonUnfoldL, Call).
unfold_call(CM:Goal, _, IsUnfold, NonUnfoldL, Call) :-
    nonvar(Goal), !,
    unfold_call(Goal, CM, IsUnfold, NonUnfoldL, Call).
unfold_call(Goal0, CM, IsUnfold, NonUnfoldL, Call) :-
    CMGoal0 = CM:Goal0,
    implementation_module(CMGoal0, M),
    qualify_meta_goal(CMGoal0, Goal),
    ( unfold_call_hook(Goal, M, CM, Goal2)
    *->
      unfold_call(Goal2, CM, IsUnfold, NonUnfoldL, Call)
    ; \+ \+ memberchk(M:Goal, NonUnfoldL)
    ->Call = CM:Goal
    ; \+ call(IsUnfold, Goal, M),
      \+ ( predicate_property(CM:Goal, meta_predicate(S)),
           arg(_, S, 0 )
         )
    ->Call = CM:Goal
    ; ( nth_clause(CM:Goal, _Idx, Ref),
        clause(M:Head, Body, Ref),
        Body \== call(Head)
      *->
        clause_property(Ref, module(BM)),
        ( subsumes_term(Head, Goal)
        ->Goal = Head,
          unfold_call(Body, BM, IsUnfold, NonUnfoldL, Call)
        ; \+ Head \= Goal
        ->copy_term(Goal, Head),
          unfold_call(Body, BM, IsUnfold, [M:Head|NonUnfoldL], Call)
          % Abstraction to get more info
        )
      ; predicate_property(CM:Goal, meta_predicate(Spec)),
        ( arg(N, Spec, S),
          arg(N, Goal, A),
          S = 0,
          unfold_call(A, CM, IsUnfold, NonUnfoldL, Call)
        )
      )
    ).
