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

:- module(neck, [neck/0]).

:- use_module(library(compound_expand)).
:- use_module(library(sequence_list)).
:- use_module(library(list_sequence)).

%!  neck is det
%
%   Stablish that everything above it should be evaluated at compile time, be
%   careful since such part should contain only predicates already defined.  In
%   case of non-determinism, several clauses would be generated.  This is a
%   practical way to generate automatic clauses with a proper instantiation of
%   the head. If neck can not be expanded, it will succeed without side effects.

neck.

term_expansion((Head :- Body1), ClauseL) :-
    sequence_list(Body1, List, []),
    append(Left, [neck|Right], List),
    '$current_source_module'(M),
    list_sequence(Left, Static),
    once(( append(LRight, RRight, Right),
           \+ ( sub_term(Lit, RRight),
                nonvar(Lit),
                Lit == !
              ) % We can not move the part above a cut to a separate clause
         )),
    ( RRight = [_, _|_]
    ->term_variables(t(Head, Static, LRight), VarHU),
      sort(VarHU, VarHL),
      term_variables(RRight, VarBU),
      sort(VarBU, VarBL),
      ord_intersection(VarHL, VarBL, ArgNB),
      functor(Head, F, A),
      variant_sha1(ArgNB-RRight, Hash),
      format(atom(FNB), '__aux_neck_~w/~d_~w', [F, A, Hash]),
      SepHead =.. [FNB|ArgNB],
      list_sequence(RRight, SepBody),
      '$expand':compile_auxiliary_clause(M, (SepHead :- SepBody)),
      append(LRight, [SepHead], NeckL),
      list_sequence(NeckL, NeckBody)
    ; list_sequence(Right, NeckBody)
    ),
    findall((Head :- NeckBody), M:Static, ClauseL).
