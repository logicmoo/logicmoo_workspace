/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
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

:- module(near_utils,
          [fact_near/1,
           fact_near/2,
           retract_near/1,
           retractall_near/1,
           real_near/2,
           real_compare/3,
           near_compare/3,
           unify_near/2,
           repsilon/1,
           repsilon/2]).

:- use_module(library(mapargs)).
:- use_module(library(compare_eq)).

:- meta_predicate
        fact_near(0 ),
        fact_near(0, -),
        retract_near(0 ),
        retractall_near(0 ).

fact_near(M:Call) :-
    freeze_near(Call, Mask),
    M:Mask,
    frozen_near(Mask).

fact_near(M:Call, Ref) :-
    freeze_near(Call, Mask),
    clause(M:Mask, _, Ref),
    frozen_near(Mask).

retract_near(M:Call) :-
    fact_near(M:Call, Ref),
    erase(Ref).

retractall_near(M:Call) :-
    forall(( freeze_near(Call, Mask),
             clause(M:Mask, _, Ref)
           ),
           erase(Ref)).

real(R) :-
    ( R == 1.5NaN
    ->fail
    ; float(R)
    ->true
    ; rational(R),
      \+ integer(R)
    ).

rnum(R) :-
    ( R == 1.5NaN
    ->fail
    ; float(R)
    ->true
    ; rational(R)
    ).

attr_unify_hook(near(Arg1), Arg) :-
    rnum(Arg),
    real_near(Arg1, Arg).

put_near(Arg1, Arg) :-
    ( nonvar(Arg1)
    ->put_attr(Arg, near_utils, near(Arg1))
    ; Arg = Arg1
    ).

freeze_near(Arg1, Arg) :-
    ( real(Arg1)
    ->put_near(Arg1, Arg)
    % ; % If at least one element of a list is real, and all the instantiated
    %   % elements are rational or floats, then freeze the near unification
    %   is_list(Arg1),
    %   once(( member(R, Arg1),
    %          real(R)
    %        )),
    %   forall(( member(A, Arg1),
    %            nonvar(A)
    %          ), rnum(A))
    % ->maplist(put_near, Arg1, Arg)
    ; var(Arg1)
    ->Arg = Arg1
    ; mapargs(freeze_near, Arg1, Arg)
    ).

frozen_near(Mask) :-
    term_attvars(Mask, Vars),
    maplist(frozen_near_1, Vars).

frozen_near_1(Var) :-
    ( get_attr(Var, near_utils, near(Val))
    ->del_attr(Var, near_utils),
      Var = Val
    ; true
    ).

real_near(A, B) :- near_compare(=, A, B).

real_compare(A, C, B) :- near_compare(C, A, B).

repsilon(E) :- E is 1024*epsilon.

repsilon(N, E) :-
    repsilon(R),
    E is R*N.

near_compare(Comparator, A, B) :-
    ( A =:= B
    ->compare_eq(Comparator)
    ; repsilon(max(abs(A), abs(B)), E),
      compare(Comparator, A, B, E)
    ).

compare(=,  A, B, E) :- abs(A - B) =< E.
compare(=<, A, B, E) :- A - B =< E.
compare(>=, A, B, E) :- B - A =< E.
compare(<,  A, B, E) :- B - A >  E.
compare(>,  A, B, E) :- A - B >  E.
compare(\=, A, B, E) :- abs(A - B) > E.

unify_near(Arg1, Arg2) :-
    ( real(Arg1),
      real(Arg2)
    ->real_near(Arg1, Arg2)
    ; ( var(Arg1)
      ; var(Arg2)
      )
    ->Arg1 = Arg2
    ; mapargs(unify_near, Arg1, Arg2)
    ).
