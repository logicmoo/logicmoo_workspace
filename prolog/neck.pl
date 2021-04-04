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

current_seq_lit(Seq, Lit, Left, Right) :-
    current_seq_lit(Seq, Lit, true, Left, true, Right).

conj(T, C, C) :- T == true.
conj(C, T, C) :- T == true.
conj(A, B, (A, B)).

current_seq_lit(S, _, _, _, _, _) :-
    var(S),
    !,
    fail.
current_seq_lit(S, S, L, L, R, R).
current_seq_lit((H, T), S, L1, L, R1, R) :-
    ( once(conj(T, R1, R2)),
      current_seq_lit(H, S, L1, L, R2, R)
    ; once(conj(L1, H, L2)),
      current_seq_lit(T, S, L2, L, R1, R)
    ).

term_expansion_hb(Head, Body1, NeckBody, Pattern, ClauseL) :-
    % (var(Head)->gtrace;true),
    '$current_source_module'(M),
    once(( current_seq_lit(Body1, Neck, Static, Right),
           memberchk(Neck, [neck, neck(X, X), necki, necki(X, X)])
         )),
    once(( current_seq_lit(Right, !, LRight, SepBody),
           \+ current_seq_lit(SepBody, !, _, _)
           % We can not move the part above a cut to a separate clause
         ; LRight = true,
           SepBody = Right
         )),
    term_variables(Head, HVars),
    '$expand':mark_vars_non_fresh(HVars),
    expand_goal(M:Static, Expanded),
    ( memberchk(Neck, [neck, neck(_, _)]),
      nonvar(SepBody),
      SepBody = (_, _),
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
      conj(LRight, SepHead, NeckBody),
      findall(Pattern, Expanded, ClauseL1),
      ( '$get_predicate_attribute'(M:SepHead, defined, 1)
      ->true
      ; ClauseL1 \= [_]
      )
    ->( '$get_predicate_attribute'(M:SepHead, defined, 1)
      ->ClauseL = ClauseL1
      ; phrase(( ( {nonvar(F)}
                 ->[(:- discontiguous IM:F/A)]
                 ; []
                 ),
                 [(SepHead :- ExpBody)|ClauseL1]
               ), ClauseL)
      )
    ; expand_goal(M:Right, M:NeckBody),
      findall(Pattern, Expanded, ClauseL)
    ).

term_expansion((Head :- Body), ClauseL) :-
    term_expansion_hb(Head, Body, NB, (Head :- NB), ClauseL).
term_expansion((Head --> Body), ClauseL) :-
    current_seq_lit(Body, Neck, _, _),
    memberchk(Neck, [neck, necki]),
    dcg_translate_rule((Head --> Body), _, (H :- B), _),
    term_expansion_hb(H, B, NB, (H :- NB), ClauseL).
term_expansion((:- Body), ClauseL) :-
    term_expansion_hb(decl, Body, NB, (:- NB), ClauseL).
