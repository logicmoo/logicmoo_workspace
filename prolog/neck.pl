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

:- use_module(library(lists)).
:- use_module(library(occurs)).
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
    '$current_source_module'(M),
    sequence_list(Body1, List, []),
    append(Left, [neck|Right], List),
    list_sequence(Left, Static),
    once(( append(LRight, RRight, Right),
           \+ ( sub_term(Lit, RRight),
                nonvar(Lit),
                Lit == !
              ) % We can not move the part above a cut to a separate clause
         )),
    term_variables(Head, HVars),
    '$expand':mark_vars_non_fresh(HVars),
    expand_goal(M:Static, Expanded),
    ( RRight = [_, _|_],
      list_sequence(RRight, SepBody),
      expand_goal(M:SepBody, M:ExpBody),
      term_variables(t(Head, Expanded, LRight), VarHU),
      '$expand':remove_var_attr(VarHU, '$var_info'),
      sort(VarHU, VarHL),
      term_variables(ExpBody, VarBU),
      sort(VarBU, VarBL),
      ord_intersection(VarHL, VarBL, ArgNB),
      strip_module(M:Head, IM, Pred),
      functor(Pred, F, A),
      variant_sha1(ArgNB-ExpBody, Hash),
      format(atom(FNB), '__aux_neck_~w:~w/~d_~w', [IM, F, A, Hash]),
      SepHead =.. [FNB|ArgNB],
      append(LRight, [SepHead], NeckL),
      list_sequence(NeckL, NeckBody),
      findall((Head :- NeckBody), Expanded, ClauseL),
      ( '$get_predicate_attribute'(M:SepHead, defined, 1)
      ->true
      ; ClauseL \= [_]
      )
    ->( '$get_predicate_attribute'(M:SepHead, defined, 1)
      ->true
      ; '$expand':compile_aux_clauses([SepHead :- ExpBody])
      )
    ; list_sequence(Right, NeckBody),
      findall((Head :- NeckBody), Expanded, ClauseL)
    ).
