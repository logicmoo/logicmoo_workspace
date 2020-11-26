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

:- module(neck, [neck/0,
                 neck/2,
                 necki/0,
                 necki/2]).

:- use_module(library(lists)).
:- use_module(library(occurs)).
:- use_module(library(ordsets)).
:- use_module(library(sequence_list)).
:- use_module(library(list_sequence)).
:- reexport(library(compound_expand)).

%!  neck is det.
%!  necki is det.
%!  neck(L, L) is det.
%!  necki(L, L) is det.
%
%   Stablish that everything above it should be evaluated at compile time, be
%   careful since such part should contain only predicates already defined.  In
%   case of non-determinism, several clauses would be generated.  This is a
%   practical way to generate automatic clauses with a proper instantiation of
%   the head. If neck can not be expanded, it will succeed without side effects.
%   necki is used if you don't want to create ancillary predicates for the body,
%   but rather have the body inlined.

neck.

neck --> [].

necki.

necki --> [].

term_expansion_hb(Head, Body1, NeckBody, Pattern, ClauseL) :-
    '$current_source_module'(M),
    sequence_list(Body1, List, []),
    once(( append(Left, [Neck|Right], List),
           nonvar(Neck),
           memberchk(Neck, [neck, neck(X, X), necki, necki(X, X)])
         )),
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
    ( memberchk(Neck, [neck, neck(_, _)]),
      RRight = [_, _|_],
      list_sequence(RRight, SepBody),
      expand_goal(M:SepBody, M:ExpBody),
      term_variables(t(Head, Expanded, LRight), VarHU),
      '$expand':remove_var_attr(VarHU, '$var_info'),
      sort(VarHU, VarHL),
      term_variables(ExpBody, VarBU),
      sort(VarBU, VarBL),
      ord_intersection(VarHL, VarBL, ArgNB),
      variant_sha1(ArgNB-ExpBody, Hash),
      ( nonvar(Head)
      ->strip_module(M:Head, IM, Pred),
        functor(Pred, F, A),
        PI = F/A
      ; PI = 'HEAD',
        IM = M
      ),
      format(atom(FNB), '__aux_neck_~w:~w_~w', [IM, PI, Hash]),
      SepHead =.. [FNB|ArgNB],
      append(LRight, [SepHead], NeckL),
      list_sequence(NeckL, NeckBody),
      findall(Pattern, Expanded, ClauseL),
      ( '$get_predicate_attribute'(M:SepHead, defined, 1)
      ->true
      ; ClauseL \= [_]
      )
    ->( '$get_predicate_attribute'(M:SepHead, defined, 1)
      ->true
      ; '$expand':compile_aux_clauses([SepHead :- ExpBody])
      )
    ; list_sequence(Right, RSequence),
      expand_goal(M:RSequence, M:NeckBody),
      findall(Pattern, Expanded, ClauseL)
    ).

term_expansion((Head :- Body), ClauseL) :-
    term_expansion_hb(Head, Body, NB, (Head :- NB), ClauseL).
term_expansion((Head --> Body), ClauseL) :-
    sequence_list(Body, List, []),
    append(_, [Neck|_], List),
    Neck == neck,
    dcg_translate_rule((Head --> Body), _, (H :- B), _),
    term_expansion_hb(H, B, NB, (H :- NB), ClauseL).
term_expansion((:- Body), ClauseL) :-
    term_expansion_hb(decl, Body, NB, (:- NB), ClauseL).
