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

:- module(location_utils,
          [property_location/3, predicate_location/2, record_location_dynamic/3,
           in_dir/2, all_call_refs/5, record_location_meta/5, record_location/4,
           in_set/2, from_location/2, property_from/3, record_location_goal/6,
           cleanup_loc_dynamic/4]).

:- use_module(library(lists)).
:- use_module(library(prolog_codewalk), []).
:- use_module(library(clambda)).
:- use_module(library(normalize_head)).
:- use_module(library(database_fact)).
:- use_module(library(extra_location)).
:- use_module(library(static_strip_module)).
:- use_module(library(compact_goal)).
:- use_module(library(predicate_from)).

from_location(From, Location) :-
    '$messages':swi_location(From, Location, []),
    Location \= [],
    !.
from_location(From, From).

in_set(FileL, File) :-
    memberchk(File, FileL).

in_dir(DirL, File) :-
    member(Dir, DirL),
    directory_file_path(Dir, _, File),
    !.

% For preds + decls
property_location(Prop, Declaration, Location) :-
    property_from(Prop, Declaration, From),
    from_location(From, Location).

% non det
property_from(Head, Declaration, From) :-
    ( dec_location(Head, Declaration, From)
    ; def_location(Head, Declaration, From)
    ).

dec_location(Head1/0, Declaration, From) :-
    normalize_head(Head1, M:Head),
    extra_location(Head, M, Declaration, From).
dec_location(M:Head1, Declaration, From) :-
    normalize_head(M:Head1, MHead),
    strip_module(MHead, N, Head),
    extra_location(Head, N, Declaration, From).

clause_from(Ref, clause(Ref)).

def_location(Head/I, clause(I), From) :-
    normalize_head(Head, P),
    nth_clause(P, I, Ref),
    clause_from(Ref, From).
def_location(M:Head, Declaration, From) :-
    normalize_head(M:Head, P),
    predicate_properties(P, List),
    ( List = []
    ->Declaration = predicate
    ; Declaration = predicate(List)
    ),
    predicate_from(P, From).

:- meta_predicate predicate_location(:,-).

predicate_location(P, Loc) :-
    predicate_from(P, From),
    from_location(From, Loc).

:- meta_predicate predicate_properties(:,-).
predicate_properties(P, List) :-
    findall(Prop,
            ( predicate_property(P, Prop),
              \+ memberchk(Prop, [interpreted,
                                  visible,
                                  built_in,
                                  defined,
                                  nodebug,
                                  number_of_rules(_),
                                  number_of_clauses(_),
                                  imported_from(_),
                                  file(_),
                                  indexed(_),
                                  last_modified_generation(_),
                                  line_count(_)])
            ), List).

prop_t(use). % In some cases is already tracked by prolog:called_by/4@database_fact
prop_t(def).
prop_t(dec).

all_call_refs(lit,  Goal,  _, CM, CM:Goal).
all_call_refs(Prop, Goal, IM, CM, CM:Fact) :-
    prop_t(Prop),
    database_fact(Prop, IM:Goal, Fact).

record_location_callable(Head, CM, Type, Call, _, From) :-
    callable(Head),
    ground(CM),
    predicate_property(CM:Head, implementation_module(M)),
    compact_goal(Call, Comp),
    record_location_goal(Head, M, Type, CM, Comp, From).

record_location_goal(Head, M, Type, CM, Call, From) :-
    record_location(Head, M, dynamic(Type, CM, Call), From).

record_location(Head, M, Type, From) :-
    ( loc_dynamic(Head, M, Type, From)
    ->true
    ; assertz(loc_dynamic(Head, M, Type, From))
    ).

record_location_meta_each(MCall, M, From, FactBuilder, Recorder) :-
    static_strip_module(MCall, M, Call, CM),
    predicate_property(MCall, implementation_module(IM)),
    call(FactBuilder, Type, Call, IM, CM, MFact),
    static_strip_module(MFact, CM, Fact, FM),
    call(Recorder, Fact, FM, Type, IM:Call, CM, From).

:- meta_predicate record_location_meta(+,?,+,5,6).
record_location_meta(MCall, M, From, FactBuilder, Recorder) :-
    \+ ( record_location_meta_each(MCall, M, From, FactBuilder, Recorder)
       *->
         fail
       ; true
       ).

record_location_dynamic(MCall, M, From) :-
    record_location_meta(MCall, M, From, \T^G^MG^_^F^database_fact_ort(T,G,MG,F),
                         record_location_callable).

cleanup_loc_dynamic(Head, M, Type, From) :-
    retractall(loc_dynamic(Head, M, Type, From)).
