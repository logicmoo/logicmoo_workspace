/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(prolog_signature,
          [ goal_signature/2,           % :Goal, -Signature
            goal_signature/3,           % :Goal, -Signature, -Vars
            goal_provenance/2,          % :Goal, -Provenance
            deep_predicate_hash/2,      % :Head, -Hash
            predicate_callees/2,        % :Head, -Callees
            predicate_dependencies/2,   % :Head, -Dependencies

            sig_clean_cache/0,
            sig_clean_cache/1           % +Module
          ]).
:- use_module(library(prolog_codewalk)).
:- use_module(library(ordsets)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(solution_sequences)).

:- meta_predicate
    goal_signature(:, -),
    goal_signature(:, -, -),
    goal_provenance(:, -),
    predicate_callees(:, -),
    deep_predicate_hash(:, -),
    predicate_dependencies(:, -).

:- multifile
    hook_predicate_hash/2.              % :Head, -Hash

/** <module> Create signatures for a program

This module is concerned with creating   signatures for a predicate. The
signature guarantees that neither the predicate   itself, not one of its
callees has changed. This is used to support persistent result caching.
*/

%!  goal_signature(:Goal, -Term) is det.
%!  goal_signature(:Goal, -Term, -Vars) is det.
%
%   Replace the module and functor of  Goal   with  a hash. For example,
%
%       user:between(1, 5, X),
%
%   becomes something like this:
%
%       '931be36e3ed89e766d332277a61664ff3c08d56a'(1, 5, X).
%
%   The hash is based on the   predicate and predicates reachable though
%   the call graph for the most generic form.
%
%   @arg Vars is a term holding the variables in Goal/Term (these are
%   the same).

:- dynamic goal_signature_c/3.

goal_signature(M:Goal, Term) :-
    goal_signature_c(Goal, M, Term0),
    predicate_dependencies_not_changed(M:Goal),
    !,
    Term = Term0.
goal_signature(M:Goal, Term) :-         % non-predicate calls
    goal_meta_head(M:Goal, Head),
    !,
    retractall(goal_signature_c(Goal, M, _)),
    setup_call_cleanup(
        asserta(M:(Head :- Goal), Ref),
        goal_signature(M:Head, Term0),
        erase(Ref)),
    assertz(goal_signature_c(Goal, M, Term0)),
    Term = Term0.
goal_signature(Goal0, Term) :-
    generalise(Goal0, M:Goal),
    retractall(goal_signature_c(Goal, M, _)),
    goal_signature_nc(M:Goal, Term0),
    assertz(goal_signature_c(Goal, M, Term0)),
    _:Goal = Goal0,
    Term = Term0.

%!  goal_meta_head(:Goal, -Head) is semidet.
%
%   True when Goal is a meta-predicate, and   Head is an artificial head
%   created from the variables in Goal and the name `'<head>'`.

goal_meta_head(M:Goal, Head) :-
    predicate_property(M:Goal, meta_predicate(MHead)),
    arg(_, MHead, Marg),
    integer(Marg),
    !,
    term_variables(Goal, Vars),
    Head =.. ['<head>'|Vars].


goal_signature_nc(M:Goal, Term) :-
    deep_predicate_hash(M:Goal, Hash),
    Goal =.. [_|Args],
    Term =.. [Hash|Args].

goal_signature(Goal, Term, Vars) :-
    goal_signature(Goal, Term),
    term_variables(Term, VarList),
    Vars =.. [v|VarList].

%!  goal_provenance(:Goal, -Provenance) is det.
%
%   Establish  the  provenance  information  for   computing  Goal.  The
%   provenance consists of a list of files and, for each file, a list of
%   dicts that describe a predicate.

goal_provenance(M:Goal, Provenance) :-
    goal_meta_head(M:Goal, Head),
    !,
    setup_call_cleanup(
        asserta(M:(Head :- Goal), Ref),
        goal_provenance(M:Head, Provenance),
        erase(Ref)).
goal_provenance(Goal, Provenance) :-
    predicate_dependencies(Goal, Callees),
    maplist(predicate_provenance, Callees, ByPredicate),
    append(ByPredicate, FlatByPredicate),
    keysort(FlatByPredicate, ByPredicateSorted),
    group_pairs_by_key(ByPredicateSorted, Provenance).

predicate_provenance(Head, Pairs) :-
    predicate_hash(Head, Hash),
    predicate_source(Head, Files),
    Dep = predicate{head:Head,
                    hash:Hash},
    file_pairs(Files, Dep, Pairs).

file_pairs([], _, []).
file_pairs([H|T0], Dep, [H-Dep|T]) :-
    file_pairs(T0, Dep, T).

predicate_source(Head, Files) :-
    predicate_property(Head, multifile),
    !,
    findall(File, distinct(File, predicate_file(Head, File)), Files).
predicate_source(Head, [File]) :-
    predicate_property(Head, file(File)),
    !.
predicate_source(Head, Files) :-
    predicate_property(Head, dynamic),
    !,
    (   Head = _:PHead,
        functor(PHead, '<head>', _)
    ->  Files = []
    ;   Files = ['<dynamic>']
    ).
predicate_source(_, ['<unknown>']).

predicate_file(Head, File) :-
    nth_clause(Head, _, Clause),
    clause_property(Clause, file(File)).


%!  deep_predicate_hash(:Head, -Hash) is det.
%
%   Compute the predicate hash of Head and   all its callees and combine
%   this into a single hash.
%
%   @tbd Could be faster by  keeping   track  of  the combined dependent
%   hashes of predicates per module.

deep_predicate_hash(Head, Hash) :-
    predicate_dependencies(Head, Callees),
    maplist(predicate_hash, Callees, Hashes),
    variant_sha1(Hashes, Hash).

%!  predicate_hash(:Head, -Hash) is det.
%
%   Compute the hash for a single   predicate. If the predicates clauses
%   can be accessed, this is the variant  hash of all clauses, otherwise
%   it is the variant hash of the head.
%
%   This predicate can be hooked using hook_predicate_hash/2.

%!  hook_predicate_hash(:Head, -Hash) is semidet.
%
%   Hook that can be used to define   the signature of a predicate. Hash
%   must be an SHA1 hash key   (see  variant_sha1/2). Defining this hook
%   has two effects:
%
%     1. The predicate is claimed to have no dependencies.  This
%        in itself can be exploited to prune dependency tracking.
%     2. The signature is Hash.  A typical use case is a fact base
%        that is derived from a file.

:- dynamic predicate_hash_c/4.

predicate_hash(Head, Hash) :-
    hook_predicate_hash(Head, Hash),
    !.
predicate_hash(M:Head, Hash) :-
    predicate_hash_c(Head, M, Gen, Hash0),
    predicate_generation(M:Head, Gen),
    !,
    Hash = Hash0.
predicate_hash(M:Head, Hash) :-
    retractall(predicate_hash_c(Head, M, _, _)),
    predicate_hash_nc(M:Head, Hash0),
    predicate_generation(M:Head, Gen),
    assertz(predicate_hash_c(Head, M, Gen, Hash0)),
    Hash = Hash0.

predicate_hash_nc(Head, Hash) :-
    implementation(Head, Head1),
    (   predicate_property(Head1, interpreted)
    ->  Head1 = _:Head2,
        findall((Head2:-Body), clause(Head1,Body), Clauses),
        variant_sha1(Clauses, Hash)
    ;   variant_sha1(Head1, Hash)
    ).

implementation(M0:Head, M:Head) :-
    predicate_property(M0:Head, imported_from(M1)),
    !,
    M = M1.
implementation(Head, Head).

:- dynamic
    predicate_dependencies_mc/3,
    predicate_dependencies_c/3.

%!  predicate_dependencies_not_changed(:Head) is semidet.
%
%   True when the dependencies of a predicate may have been changed.

predicate_dependencies_not_changed(M:Head) :-
    predicate_dependencies_mc(Head, M, Modules),
    maplist(module_not_modified, Modules).

%!  predicate_dependencies(:Head, -Callees:list(callable)) is det.
%
%   True when Callees is a set (ordered list) of all predicates that are
%   directly or indirectly reachable through Head.

predicate_dependencies(Goal, Callees) :-
    generalise(Goal, M:Head),
    (   hook_predicate_hash(Head, _Hash)
    ->  Callees = []
    ;   predicate_dependencies_mc(Head, M, Modules),
        predicate_dependencies_c(Head, M, Callees0),
        (   maplist(module_not_modified, Modules)
        ->  true
        ;   maplist(predicate_not_modified, Callees0)
        ->  callee_modules(Callees0, Modules),
            retractall(predicate_dependencies_mc(Head, M, _)),
            assertz(predicate_dependencies_mc(Head, M, Modules))
        )
    ->  true
    ;   retractall(predicate_dependencies_mc(Head, M, _)),
        retractall(predicate_dependencies_c(Head, M, _)),
        retractall(goal_signature_c(Head, M, _)),
        predicate_dependencies_nc(M:Head, Callees0),
        callee_modules(Callees0, Modules),
        assertz(predicate_dependencies_c(Head, M, Callees0)),
        assertz(predicate_dependencies_mc(Head, M, Modules))
    ),
    Callees = Callees0.

predicate_not_modified(M:Head) :-
    predicate_callees_c(Head, M, Gen, _Callees0),
    predicate_generation(M:Head, Gen).

module_not_modified(M-Gen) :-
    (   module_property(M, last_modified_generation(Gen0))
    ->  Gen0 == Gen
    ;   Gen == 0
    ).

callee_modules(Callees, Modules) :-
    maplist(arg(1), Callees, MList0),
    sort(MList0, MList),
    maplist(module_gen, MList, Modules).

module_gen(M, M-Gen) :-
    module_property(M, last_modified_generation(Gen)),
    !.
module_gen(M, M-0).

predicate_dependencies_nc(Head0, Callees) :-
    implementation(Head0, Head),
    ground(Head, GHead),
    predicate_dependencies(Head, [GHead], Callees0),
    maplist(generalise, Callees0, Callees1),
    order_callees(Callees1, Callees).

%!  order_callees(+Callees1, -Callees) is det.
%
%   Order the callees such that the   ordering remains consistent in the
%   presence of a temporary, anonymous module.   We  first order by Head
%   and then if there are module conflicts we place the temporary module
%   last.
%
%   @tbd an alternative might be to use the deep hash for ordering, such
%   that the hash becomes  completely   independent  from  predicate and
%   module naming.

order_callees(Callees1, Callees) :-
    sort(2, @>=, Callees1, Callees2),
    tmp_order(Callees2, Callees).

tmp_order([], []).
tmp_order([M1:H,M2:H|T0], L) :-
    tmp_module(M1),
    !,
    L = [M2:H|T],
    tmp_order([M1:H|T0], T).
tmp_order([H|T0], [H|T]) :-
    tmp_order(T0, T).

%!  predicate_dependencies(+Head, +Callees0, -Callees)
%
%   Compute the transitive  closure  of   predicates  called  from Head.
%   Predicates are represented as M:C, where C is a numbervars-ed ground
%   term.

predicate_dependencies(Head, Callees0, Callees) :-
    predicate_callees(Head, Called),
    maplist(ground, Called, GCalled),
    ord_subtract(GCalled, Callees0, New),
    (   New == []
    ->  Callees = Callees0
    ;   ord_union(Callees0, GCalled, Callees1),
        foldl(predicate_dependencies, New, Callees1, Callees)
    ).

ground(Term, Ground) :-
    generalise(Term, Term2),
    copy_term(Term2, Ground),
    numbervars(Ground, 0, _).

:- thread_local
    calls/1.

:- dynamic predicate_callees_c/4.

predicate_callees(M:Head, Callees) :-
    predicate_callees_c(Head, M, Gen, Callees0),
    predicate_generation(M:Head, Gen),
    !,
    Callees = Callees0.
predicate_callees(M:Head, Callees) :-
    retractall(predicate_callees_c(Head, M, _, _)),
    predicate_callees_nc(M:Head, Callees0),
    predicate_generation(M:Head, Gen),
    assertz(predicate_callees_c(Head, M, Gen, Callees0)),
    Callees = Callees0.

predicate_callees_nc(Head0, Callees) :-
    generalise(Head0, Head),
    findall(CRef, nth_clause(Head, _, CRef), CRefs),
    prolog_walk_code(
        [ clauses(CRefs),
          autoload(true),
          trace_reference(_:_),
          on_trace(track_ref),
          source(false)
        ]),
    findall(Callee, retract(calls(Callee)), Callees0),
    sort(Callees0, Callees).

:- public track_ref/3.

track_ref(Callee0, Caller, _Location) :-
    generalise(Callee0, Callee1),
    implementation(Callee1, Callee),
    (   calls(Callee)
    ->  true
    ;   \+ Callee \= Caller                     % exclude recursion
    ->  true
    ;   Callee = M:_,
        module_property(M, class(Class)),
        nodep_module_class(Class)
    ->  true
    ;   assertz(calls(Callee))
    ).

nodep_module_class(system).
nodep_module_class(library).


generalise(M:Head0, M:Head) :-
    functor(Head0, Name, Arity),
    functor(Head, Name, Arity).

predicate_generation(Head, Gen) :-
    predicate_property(Head, last_modified_generation(Gen0)),
    !,
    Gen = Gen0.
predicate_generation(_, 0).

%!  sig_clean_cache is det.
%!  sig_clean_cache(+M) is det.
%
%   Cleanup cached signatures and dependencies. If   a  module is given,
%   only the depedencies for the matching module are removed.

sig_clean_cache :-
    sig_clean_cache(_).

sig_clean_cache(M) :-
    retractall(goal_signature_c(_,M,_)),
    retractall(predicate_callees_c(_,M,_,_)),
    retractall(predicate_hash_c(_,M,_,_)),
    retractall(predicate_dependencies_c(_,M,_)),
    retractall(predicate_dependencies_mc(_,M,_)).

%!  tmp_module(+M) is semidet.
%
%   True if M is a module that may   be switched while the result should
%   still be the same. These are also   modules that can be removed from
%   the cache.

tmp_module(M) :-
    module_property(M, class(temporary)).


		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(prolog_signature:goal_signature/2).
sandbox:safe_meta_predicate(prolog_signature:goal_signature/3).
sandbox:safe_meta_predicate(prolog_signature:goal_provenance/2).
sandbox:safe_meta_predicate(prolog_signature:deep_predicate_hash/2).
