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

:- module(database_fact,
          [database_fact/1,
           database_fact/3,
           database_fact_ort/4,
           database_def_fact/2,
           database_mod_fact/2,
           database_use_fact/2,
           clause_head/2,
           fa_to_head/3
          ]).

:- use_module(library(assrt_lib)).
:- use_module(library(plprops)).
:- use_module(library(extend_args)).
:- use_module(library(static_strip_module)).

:- create_prolog_flag(check_database_preds, false, [type(boolean)]).

% help analyzers to track indirect calls via prolog database manipulation:
%

prolog:called_by(H, IM, CM, [F]) :-
    current_prolog_flag(check_database_preds, true),
    \+ is_meta(IM:H),
    database_use_fact(IM:H, F),
    static_strip_module(F, CM, C, M),
    callable(C),
    nonvar(M).

is_meta(G) :-
    predicate_property(G, meta_predicate(Meta)),
    arg(_, Meta, S),
    integer(S).

:- multifile
    database_def_fact/3,
    database_dec_fact/3,
    database_retract_fact/3,
    database_query_fact/3.

:- meta_predicate
    database_fact(0).

database_fact(M:G) :-
    predicate_property(M:G, implementation_module(IM)),
    database_fact(IM:G, _).
database_fact(MG) :-
    prop_asr(head, MG, _, Asr),
    prop_asr(glob, database(_), _, Asr).

database_mod_fact(M:G, F) :- database_def_fact(    G, M, F).
database_mod_fact(M:G, F) :- database_dec_fact(    G, M, F).
database_mod_fact(M:G, F) :- database_retract_fact(G, M, F).

database_use_fact(M:G, F) :- database_query_fact(  G, M, F).
database_use_fact(M:G, F) :- database_retract_fact(G, M, F).

clause_head(A,          A) :- var(A), !.
clause_head(M:A,        M:A) :- var(A), !.
clause_head((A :- _),   A) :- !.
clause_head(M:(A :- _), M:A) :- !.
clause_head(A,          A).

database_fact(def, Goal, Fact) :- database_def_fact(Goal, Fact).
database_fact(dec, Goal, Fact) :- database_dec_fact(Goal, Fact).
database_fact(use, Goal, Fact) :- database_use_fact(Goal, Fact).
database_fact(mod, Goal, Fact) :- database_mod_fact(Goal, Fact).

% ortogonal operations:
database_fact_ort(def,     G, M, F) :- database_def_fact(G, M, F).
database_fact_ort(dec,     G, M, F) :- database_dec_fact(G, M, F).
database_fact_ort(retract, G, M, F) :- database_retract_fact(G, M, F).
database_fact_ort(query,   G, M, F) :- database_query_fact(G, M, F).

database_fact(M:G, F) :- database_fact_ort(_, G, M, F).

database_def_fact(M:H, F) :- database_def_fact(H, M, F).

database_def_fact(asserta_with_names(A, _),  ifprolog,   F) :- clause_head(A, F).
database_def_fact(assertz_with_names(A, _),  ifprolog,   F) :- clause_head(A, F).
database_def_fact(lasserta(A),               pce_config, F) :- clause_head(A, F).
database_def_fact(assert_cyclic(A),          plunit,     F) :- clause_head(A, F).
database_def_fact(assert(A),                 system,     F) :- clause_head(A, F).
database_def_fact(assert(A, _),              system,     F) :- clause_head(A, F).
database_def_fact(asserta(A),                system,     F) :- clause_head(A, F).
database_def_fact(asserta(A, _),             system,     F) :- clause_head(A, F).
database_def_fact(assertz(A),                system,     F) :- clause_head(A, F).
database_def_fact(assertz(A, _),             system,     F) :- clause_head(A, F).

database_dec_fact(M:H, F) :- database_dec_fact(H, M, F).

database_dec_fact(abolish(F, A),             system,     H) :- fa_to_head(F, A, H).
database_dec_fact(abolish(PI),               system,     H) :- pi_to_head(PI, H).
database_dec_fact(retractall(F),             system,     F).
database_dec_fact(retractall_near(F),        near_utils, F).
database_dec_fact(forall(A, B),              system,     F) :-
    subsumes_term(forall(retract(F), true), forall(A, B)),
    A=retract(F).
database_dec_fact(\+ A,  system,     F) :-
    subsumes_term((retract(F), \+ true), A),
    A = (retract(F), \+ true).

database_def_fact(update_fact_from(A, From), from_utils, F) :-
    nonvar(A),
    extend_args(A, [From], H),
    clause_head(H, F).

database_retract_fact(retract(A),  system,     F) :- clause_head(A, F).
database_retract_fact(lretract(A), pce_config, F) :- clause_head(A, F).

database_query_fact(clause(A, _),       system,     F) :- clause_head(A, F).
database_query_fact(clause(A, _, _),    system,     F) :- clause_head(A, F).
database_query_fact(unfold_goal(_,A,_), refactor,   F) :- clause_head(A, F).
database_query_fact(fact_near(A, _),    near_utils, F) :- clause_head(A, F).

pi_to_head(PI, H) :- nonvar(PI) -> PI=F/A, fa_to_head(F, A, H) ; true.

fa_to_head(M:F, A, M:H) :- atomic(M) -> fa_to_head_(F, A, H), !.
fa_to_head(F,   A, H) :- fa_to_head_(F, A, H).

fa_to_head_(F, A, H) :- atomic(F), integer(A) -> functor(H, F, A) ; true.
