/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
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

:- module(called_from, [called_from/1,
                        called_from/2,
                        called_from/5,
                        collect_called_from/5,
                        collect_called_from/6,
                        current_called_from/5,
                        current_used_from/6,
                        used_predicates/2,
                        used_predicates/3
                       ]).

:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(assertions)).
:- use_module(library(normalize_head)).
:- use_module(library(normalize_pi)).
:- use_module(library(codewalk)).
:- use_module(library(extra_location)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).

:- multifile
    prolog:message//1.

:- dynamic called_from_db/5.

prolog:message(acheck(called_from(MsgLoc, Args))) -->
        MsgLoc,
        ['~w called from ~w'-Args].

called_from(Ref) :-
        called_from(Ref, _).

called_from(Ref, Caller) :-
    ( called_from(Ref, _CM, Caller, [], Sorted),
      maplist(print_call_point, Sorted),
      fail
    ; cleanup_loc_dynamic(_, _, dynamic(_, _, _), _),
      retractall(called_from_db(_, _, _, _, _))
    ).

called_from(Ref, CM, Caller, Options, Pairs) :-
    normalize_head(Ref, M:H),
    collect_called_from(H, M, CM, Caller, Options, Pairs).

collect_called_from(H, M, CM, Caller, Options, Sorted) :-
    collect_called_from(H, M, CM, Caller, Options),
    findall(Loc-[M:F/A, CPI],
            ( current_called_from(H, M, CM, From, C),
              functor(H, F, A),
              normalize_pi(C, CPI),
              from_location(From, Loc)
            ), Pairs),
    sort(Pairs, Sorted).

collect_called_from(Ref, M, CM, Caller, Options1) :-
    cleanup_loc_dynamic(_, _, dynamic(_, _, _), _),
    retractall(called_from_db(_, _, _, _, _)),
    merge_options([source(true),
                   infer_meta_predicates(false),
                   autoload(false),
                   evaluate(false),
                   method(prolog),
                   trace_reference(_:Ref),
                   module_class([user, system, library]),
                   on_trace(collect_call_point(M, CM, Caller))],
                  Options1, Options),
    walk_code(Options).

current_called_from(H, M, CM, From, Caller) :-
    current_used_from([retract, query], H, M, CM, From, Caller).

current_used_from(DynTypes, H, M, CM, From, Caller) :-
    ( called_from_db(H, M, CM, Caller, From)
    ; loc_dynamic(H, M, dynamic(Type, CM, Caller), From),
      memberchk(Type, DynTypes)
    ; loc_declaration(H, CM, goal, From),
      predicate_property(CM:H, implementation_module(M))
    ; curr_prop_asr(head, CM:H, From, _),
      predicate_property(CM:H, implementation_module(M)),
      Caller = '<assertion>'(M:H)
    ).

:- public collect_call_point/6.
:- meta_predicate collect_call_point(?, ?, ?, +, +, +).
collect_call_point(IM, M, Caller, MGoal, Caller, From) :-
    ignore(record_location_dynamic(MGoal, IM, From)),
    MGoal = M:Goal,
    predicate_property(MGoal, implementation_module(IM)),
    update_fact_from(called_from_db(Goal, IM, M, Caller), From).

print_call_point(L-A) :-
    print_message(information, acheck(called_from(L, A))).

% used_predicates(+Module, +Context, -PIL) is det
%
% Unifies PIL with a list of predicates implemented in the module Module,
% actually being used in the context Context.  Note that this would be different
% than the imported predicates.
%
used_predicates(Module, Context, PIL) :-
    collect_called_from(_, Module, Context, _, [source(false)]),
    findall(F/A,
            ( current_called_from(H, Module, Context, _, _),
              functor(H, F, A)
            ), PIU),
    sort(PIU, PIL).

used_predicates(Module, Groups) :-
    collect_called_from(_, Module, _, _, [source(false)]),
    findall(Context-(F/A),
            ( current_called_from(H, Module, Context, _, _),
              functor(H, F, A)
            ), Pairs),
    sort(Pairs, Sorted),
    group_pairs_by_key(Sorted, Groups).
