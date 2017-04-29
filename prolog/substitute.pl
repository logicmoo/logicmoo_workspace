/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- module(substitute,
          [substitute/3,
           substitute_value/4,
           substitute_values/3,
           is_subterm/2]).

:- meta_predicate substitute(2, ?, ?).
substitute(Comp, Term0, Term) :-
    ( call(Comp, Term0, Subs)
    ->Term = Subs
    ; compound(Term0),
      functor(Term0, F, A),
      functor(Term,  F, A),
      substitute(1, Comp, Term0, Term),
      %% required to avoid construction of equivalent terms:
      Term0 \== Term
    ->true
    ; Term = Term0
    ).

substitute(N, Comp, Term0, Term) :-
    arg(N, Term0, Arg0),
    !,
    substitute(Comp, Arg0, Arg),
    arg(N, Term, Arg),
    succ(N, N1),
    substitute(N1, Comp, Term0, Term).
substitute(_, _, _, _).

substitute_one(Value, Var, Term, Var) :-
    Value==Term.

substitute_value(Value, Subs, Term0, Term) :-
    substitute(substitute_one(Value, Subs), Term0, Term).

unpair_eq(V=S, V, S).

substitute_values(Pairs, Term0, Term) :-
    maplist(unpair_eq, Pairs, Values, Subss),
    foldl(substitute_value, Values, Subss, Term0, Term).

is_subterm(SubTerm, Term) :-
    substitute_value(SubTerm, Var, Term, Term1),
    occurrences_of_var(Var, Term1, N),
    !,
    N > 0.
