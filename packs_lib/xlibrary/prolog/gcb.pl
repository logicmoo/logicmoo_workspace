/*  Part of Refactoring Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.
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

:- module(gcb, [greatest_common_binding/7,
                greatest_common_binding/8]).

:- use_module(library(substitute), [substitute_value/4]).

greatest_common_binding(Term1, Into1, Term, Into, Skip) -->
    greatest_common_binding(Term1, Term1, Into1, Term, Into, Skip).

greatest_common_binding(SubTerm1, Term1, Into1, Term, Into, Skip) -->
    ( { \+memberchk(SubTerm1, Skip),
        substitute_value(SubTerm1, Var, Into1, Into2),
        Into1\==Into2
      }
    ->{substitute_value(SubTerm1, Var, Term1, Term2)},
      [Var=SubTerm]
    ; {Term2=Term1, Into2=Into1 }
    ),
    ( {compound(SubTerm1)},
      greatest_common_binding(1, SubTerm1, Term2, Into2, SubTerm, Term, Into, Skip),
      {Into2\==Into}
    ->[]
    ; {SubTerm=SubTerm1, Term=Term2, Into=Into2}
    ).

greatest_common_binding(N, SubTerm1, Term1, Into1, SubTerm, Term, Into, Skip) -->
    {arg(N, SubTerm1, Arg)},
    !,
    greatest_common_binding(Arg, SubTerm1-Term1, Into1, SubTerm2-Term2, Into2, Skip),
    {succ(N, N1)},
    greatest_common_binding(N1, SubTerm2, Term2, Into2, SubTerm, Term, Into, Skip).
greatest_common_binding(_, SubTerm, Term, Into, SubTerm, Term, Into, _) --> [].

/*
% Fixpoint algorithm:
substitute_olist(SubstList, Term1, Term) :-
    ( substitute_olist_(SubstList, Term1, Term2),
      Term1 \== Term2 ->
      substitute_olist(SubstList, Term2, Term)
    ; Term1 = Term
    ).

substitute_olist_(Tail) --> {var(Tail)}, !.
substitute_olist_([Var=Val|Tail]) -->
    substitute_value(Val, Var),
    substitute_olist_(Tail).

pick_tail(Tail, Tail, Tail).

greatest_common_binding(SubTerm1, SubTerm, Term1, Into1, Term, Into, Skip) -->
    ( { % nonvar(SubTerm1),
        % nonvar(Into1),
        \+memberchk(SubTerm1, Skip)}
    ->( { substitute_value(SubTerm1, Var, Into1, Into2),
          Into1\==Into2}
      ->{substitute_value(SubTerm1, Var, Term1, Term2)},
        [Var=SubTerm]
      ; {Term2=Term1, Into2 = Into1 }
      ),
      ( {compound(SubTerm1)},
        greatest_common_binding(1, SubTerm1, SubTerm, Term2, Into2, Term, Into, Skip),
        {Into2\==Into}
      ->[]
      ; {SubTerm=SubTerm1, Term=Term2, Into=Into2 }
      )
    ; {SubTerm=SubTerm1, Term=Term1, Into=Into1 }
    ).

greatest_common_binding(N, SubTerm1, SubTerm, Term1, Into1, Term, Into, Skip) -->
    {arg(N, SubTerm1, Arg)},
    !,
    pick_tail(Tail),
    greatest_common_binding(Arg, _, Term1, Into1, Term2, Into2, Skip),
    {substitute_olist(Tail, Term2, Term3),
     substitute_olist(Tail, SubTerm1, SubTerm2),
     succ(N, N1)},
    greatest_common_binding(N1, SubTerm2, SubTerm, Term3, Into2, Term, Into, Skip).
greatest_common_binding(_, SubTerm, SubTerm, Term, Into, Term, Into, _) --> [].

% Fixpoint algorithm:
substitute_olist(SubstList, Term1, Term) :-
    ( substitute_olist_(SubstList, Term1, Term2),
      Term1 \== Term2 ->
      substitute_olist(SubstList, Term2, Term)
    ; Term1 = Term
    ).

substitute_olist_(Tail) --> {var(Tail)}, !.
substitute_olist_([Var=Val|Tail]) -->
    substitute_value(Val, Var),
    substitute_olist_(Tail).

pick_tail(Tail, Tail, Tail).
*/
