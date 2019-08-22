/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
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

:- module(linearize, [linearize/5]).

get_substitutions(term, Term, SubsL) :-
    term_variables(Term, Vars),
    exclude(singleton(Term), Vars, SubsL).
get_substitutions(atom, Term, Atomics) :-
    findall(Atomic,
            ( sub_term(Atomic, Term),
              atomic(Atomic),
              Atomic \= [] % Avoid [] since this will cause problems
            ), Atomics).

substitutable(term, Term) :- var(Term).
substitutable(atom, Term) :- atomic(Term).

linearize(SubTermType, Term, Linear) -->
    {get_substitutions(SubTermType, Term, SubsL)},
    mklinear(Term, SubTermType, Linear, SubsL).

singleton(T, S) :- occurrences_of_var(S, T, 1).

mklinear(Term, SubTermType, Subs, SubsL) -->
    {compound(Term)},
    !,
    { compound_name_arity(Term, F, A),
      compound_name_arity(Subs, F, A)
    },
    mklinear(1, Term, SubTermType, Subs, SubsL).
mklinear(Term, SubTermType, Subs, SubsL) -->
    ( { substitutable(SubTermType, Term),
        member(Var1, SubsL),
        Term==Var1
      }
    ->[Subs=Term]
    ; {Subs=Term}
    ).

mklinear(N, Term, SubTermType, Subs, SubsL) -->
    { arg(N, Term, Arg),
      !,
      arg(N, Subs, LArg),
      succ(N, N1)
    },
    mklinear(Arg, SubTermType, LArg, SubsL),
    mklinear(N1, Term, SubTermType, Subs, SubsL).
mklinear(_, _, _, _, _) --> [].
