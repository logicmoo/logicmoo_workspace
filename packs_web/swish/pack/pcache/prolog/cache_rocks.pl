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

:- module(cache_rocks,
          [ cache_open/1,               % +Directory
            cache_close/0,              %

            cached/1,                   % :Goal
            cached/2,                   % :Goal, +Hash

            (cache_dynamic)/1,          % :Head
            cache_assert/1,             % :Fact
            cache_asserta/1,            % :Fact
            cache_assertz/1,            % :Fact
            cache_retract/1,            % :Fact
            cache_retractall/1,         % :Fact

            cache_property/2,           % :Goal, ?Property
            this_cache_property/2,      % :Goal, ?Property
            forget/1,                   % :Goal
            cache_statistics/1,         % ?Property
            cache_listing/0,            %
            cache_listing/1,            % +Options

            op(1150, fx, (cache_dynamic))
          ]).
:- use_module(library(rocksdb)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(record)).
:- use_module(library(settings)).
:- use_module(signature).

/** <module> Persistent answer caching

This module implements persistent caching   of  answers. The inspiration
comes from tabled execution where tabled answers   are kept as a trie of
tries. The outer trie maps goal variants  to answer tries and the answer
tries  provide  the  answers   to   a    specific   goal   variant.  The
library(rocksdb) (and library(bdb)) provide a persistent Key-Value store
that can map a term to a term.   The term is represented as an _external
record_, basically a  binary  alternative   to  write/read.  This binary
representation is a _blob_ for the   key-value store. The representation
represents a variant, currently with two limitations:

  1. If the term has internal sharing, it is different from a term
     without.  E.g., `A = f(X), B = b(A,A)` is a different `B` than
     you get from `B = b(f(X),f(X))`.
  2. If the key is cyclic, it only matches internally equivalent
     cycles.  E.g., `A = [a|A]` and `A = [a,a|A]` are considered
     different.

Ignoring these two issues (which can be   fixed),  we can use RocksDB or
BDB as the _outer_ trie used in tabling.  We could use a trie or similar
structure for the set of answers, but in  this case a list preserves the
original order and is more  compact.   Our  database basically maps call
variants to a list of answers.

In  addition,  it  does  some  book  keeping.  First  of  all,  it  uses
signature.pl to compute a _deep hash_ of   the predicate. A deep hash is
an SHA1 hash computed  from  the  clauses   of  the  predicates  and all
predicates called by  it.  The  original   goal,  say  m:p(a1,  ...)  is
translated into <SHA1>(a1, ...). This implies  that changing a predicate
or one of the predicates called by   it invalidate the cache. Second, it
keeps track of partially completed  goals   and  fully  completed goals.
Re-running a fully completed goal simply   retrieves the cached answers.
Re-running a partially completed goal first retrieves the cached answers
and then re-runs the goal with an  offset to compute additional answers,
updating the status.
*/

:- meta_predicate
    cached(0),
    cached(:, +),
    cache_dynamic(:),
    cache_assert(:),
    cache_asserta(:),
    cache_assertz(:),
    cache_retract(:),
    cache_retractall(:),
    forget(:),
    this_cache_property(:, ?),
    cache_property(:, ?),

    offset_check(+, 0, +),
    rocks_variant(:, ?).

:- dynamic
    rocks_d/2,
    rocks_variant_c/3,                      % Signature, Module, Goal
    rocks_predicate_c/2,                    % Module, Goal
    rocks_variant_cache/0,
    rocks_variant_cache_enabled/0.

:- setting(merge, boolean, false,
           "Perform updates using merge (true) or put (default)").

%!  cache_open(+Directory) is det.
%
%   Open an answer cache in  Directory.   If  Directory  does not exist,
%   create it as an empty answer   store.  Otherwise re-open an existing
%   store.

cache_open(Dir) :-
    rocks_d(_, Dir),
    !.
cache_open(Dir) :-
    rocks_d(_, _),
    permission_error(open, cache, Dir).
cache_open(Dir) :-
    rocks_open(Dir, DB,
               [ key(term),
                 value(term),
                 merge(merge_dynamic)
               ]),
    asserta(rocks_d(DB, Dir)).

%!  cache_close is det.
%
%   Close an previously opened  persistent   cache  directory.  Succeeds
%   without error if no cache  is  open.   May  raise  RocksDB errors if
%   closing fails.

cache_close :-
    forall(retract(rocks_d(DB, _Dir)),
           rocks_close(DB)),
    retractall(rocks_variant_cache),
    retractall(rocks_variant_cache_enabled),
    retractall(rocks_variant_c(_,_,_)),
    retractall(rocks_predicate_c(_,_)).

rocks(DB) :-
    rocks_d(DB, _),
    !.
rocks(_) :-
    throw(error(pcache(no_db), _)).

:- record cache(variant,                % M:Goal term
                answers,                % List of v(V1,V2,...) terms
                state,                  % complete, partial or exception
                time,                   % time stamp created
                hash).                  % hash of the variant

%!  cached(:Goal)
%
%   This predicate is logically equivalent to Goal. However, answers are
%   on the first call collected in a   trie and subsequently returned in
%   arbitrary (hash key) order without duplicates.

cached(G) :-
    rocks(DB),
    goal_signature(G, Signature, Vars),
    (   rocks_get(DB, Signature, cache(G, Answers, State, _Time, _Hash))
    ->  from_db(State, Vars, Answers, restart(G, Signature, DB))
    ;   generalise_goal(G, 2, General, Bindings),
        goal_signature(General, GenSignature, GenVars),
        rocks_get(DB, GenSignature,
                  cache(GenGoal, GenAnswers, State, Time, Hash))
    ->  debug(cache(subsumes), 'Filtering ~p for ~p', [GenGoal, G]),
        maplist(bind, Bindings),
        findall(Vars, member(GenVars, GenAnswers), Answers),
        rocks_put(DB, Signature, cache(G, Answers, State, Time, Hash)),
        member(Vars, Answers)
    ;   cache(G, Signature, Vars, [], DB)
    ).

bind(V=V).

cache(G, Sign, Vars, Sofar, DB) :-
    get_time(Now),
    copy_term(G+Sign, Goal+Signature),
    functor(Signature, Hash, _),
    Cache = cache(Goal, _Answers, _State, Now, Hash),
    (   Sofar == []
    ->  Enum = (G, add_answer(Set, Vars)),
        Update = new
    ;   Enum = (offset_check(Vars, G, Sofar), add_answer(Set, Vars)),
        Update = resume
    ),
    setup_call_catcher_cleanup(
        answer_set(Sofar, Set),
        Enum,
        Catcher,
        commit(Catcher, Set, Signature, Cache, Update, DB)).

commit(exit, Set, Signature, Cache, Update, DB) :-
    answers(Set, Answers),
    set_cache(Cache, Answers, complete),
    rocks_put(DB, Signature, Cache),
    register_variant(Update, Signature, Cache).
commit(fail, Set, Signature, Cache, Update, DB) :-
    answers(Set, Answers),
    set_cache(Cache, Answers, complete),
    rocks_put(DB, Signature, Cache),
    register_variant(Update, Signature, Cache).
commit(!, Set, Signature, Cache, Update, DB) :-
    answers(Set, Answers),
    set_cache(Cache, Answers, partial),
    rocks_put(DB, Signature, Cache),
    register_variant(Update, Signature, Cache).
commit(exception(E), Set, Signature, Cache, Update, DB) :-
    answers(Set, Answers),
    set_cache(Cache, Answers, exception(E)),
    rocks_put(DB, Signature, Cache),
    register_variant(Update, Signature, Cache).
commit(external_exception(_), Set, Signature, Cache, Update, DB) :-
    answers(Set, Answers),
    set_cache(Cache, Answers, partial),
    rocks_put(DB, Signature, Cache),
    register_variant(Update, Signature, Cache).

from_db(complete, Vars, Answers, _Restart) :-
    member(Vars, Answers).
from_db(partial, Vars, Answers, restart(G, Signature, DB)) :-
    (   member(Vars, Answers)
    ;   cache(G, Signature, Vars, Answers, DB)
    ).
from_db(exception(E), Vars, Answers, _Restart) :-
    (   member(Vars, Answers)
    ;   throw(E)
    ).


set_cache(cache(_Goal, Answers, State, _Now, _Hash), Answers, State).

%!  answer_set(+List0, -Answers) is det.
%!  add_answer(+Answers, +Answer) is det.
%!  answers(+Answers, -List) is det.

answer_set([], answers(List, List)) :-
    !,
    List = [$|_].
answer_set(Set0, answers([$|OpenSet], Tail)) :-
    open_list(Set0, OpenSet, Tail2),
    Tail = Tail2.                       % delay unification to avoid
                                        % Tail becoming a reference chain
open_list([Last], T, T) :-
    !,
    T = [Last|_].
open_list([H|T0], [H|T], Last) :-
    open_list(T0, T, Last).

add_answer(Set, A) :-
    arg(2, Set, T0),
    duplicate_term(A, A2),
    nb_linkarg(2, T0, [A2|_]),
    arg(2, T0, T),
    nb_linkarg(2, Set, T).

answers(answers([$|Answers], T), Answers) :-
    arg(2, T, []).

%!  offset_check(+Template, :Goal, +Expected)
%
%   Skip the first Expected answers from Goal.   Raise an exception if a
%   produced answer is not a variant of Template.

offset_check(Template, Goal, Expected) :-
    State = state(Expected),
    Goal,
    arg(1, State, Answers),
    (   Answers == []
    ->  true
    ;   Answers = [First|More],
        (   First =@= Template
        ->  nb_linkarg(1, State, More),
            fail
        ;   throw(error(consistency_error(Goal, Template, First),_))
        )
    ).

%!  generalise_goal(:Goal, +MaxDepth, -General, -Bindings) is nondet.
%
%   True  when  General  is  a  generalization    of  Goal  achieved  by
%   generalising terms in Bindings. Bindings  is   a  list of `Term=Var`
%   pairs.  Generalization  first  turns  a  compound  entirely  into  a
%   variable  before  preserving  the  functor    and  generalizing  the
%   arguments.
%
%   @arg MaxDepth defines how deep we   look into arguments for possible
%   generalizations. When `1`, we  merely   replace  nonvar arguments of
%   Goal with a variable.

generalise_goal(M:G0, MaxDepth, M:G, Bindings) :-
    generalise(MaxDepth, G0, G, [], Bindings),
    nonvar(G),
    G0 \== G.

generalise(_, Term, Term, Bindings, Bindings).
generalise(_, Term, Var, Bindings0, Bindings) :-
    nonvar(Term),
    Bindings = [Term=Var|Bindings0].
generalise(MaxDepth, Term, Gen, Bindings0, Bindings) :-
    succ(MaxDepth2, MaxDepth),
    compound(Term),
    compound_name_arguments(Term, Name, Args0),
    foldl(generalise(MaxDepth2), Args0, Args, Bindings0, Bindings),
    compound_name_arguments(Gen, Name, Args),
    Gen \== Term.

%!  cached(:Goal, +Hash)
%
%   Get the answers for Goal from an   old hashed result. Hash is either
%   the full hash or a _shorthash_ (7 character prefix).

cached(Goal, HashS) :-
    atom_string(Hash, HashS),
    is_hash(Hash, Type),
    rocks(DB),
    cached(Type, error, DB, Goal, Hash).

cached(sha1, OnError, DB, M:Goal, Hash) :-
    (   Goal =.. [_|Args],
        Signature =.. [Hash|Args],
        rocks_get(DB, Signature,
                  cache(M:Goal, Answers, _State, _Time, _Hash))
    ->  term_variables(Goal, VarList),
        Vars =.. [v|VarList],
        member(Vars, Answers)
    ;   generalise_goal(M:Goal, 5, M:General, Bindings),
        General =.. [_|Args],
        Signature =.. [Hash|Args],
        rocks_get(DB, Signature,
                  cache(M:GenGoal, GenAnswers, _State, _Time, _Hash))
    ->  debug(cache(subsumes), 'Filtering ~p for ~p', [GenGoal, Goal]),
        term_variables(General, VarList),
        GenVars =.. [v|VarList],
        maplist(bind, Bindings),
        member(GenVars, GenAnswers)
    ;   OnError == fail
    ->  fail
    ;   existence_error(answer_cache, Hash)
    ).
cached(short, _, DB, M:Goal, ShortHash) :-
    (   callable(Goal)
    ->  functor(Goal, Name, Arity),
        functor(Variant, Name, Arity)
    ;   true
    ),
    rocks_variant(M:Variant, Signature),
    functor(Signature, Hash, _),
    sub_atom(Hash, 0, _, _, ShortHash),
    !,
    rocks_get(DB, Signature, Cache),
    cache_variant(Cache, Variant),
    cache_answers(Cache, Answers),
    (   Goal =@= Variant
    ->  term_variables(Goal, VarList),
        Vars =.. [v|VarList],
        member(Vars, Answers)
    ;   subsumes_term(Variant, Goal)
    ->  term_variables(Variant, VarList),
        GenVars =.. [v|VarList],
        Goal = Variant,
        member(GenVars, Answers)
    ;   throw(error(specific_expected(Goal, Variant, ShortHash), _))
    ).

is_hash(Atom, Hash) :-
    atom_length(Atom, Len),
    (   Len == 40
    ->  Hash = sha1
    ;   Len == 7
    ->  Hash = short
    ;   domain_error(hash, Atom)
    ).


		 /*******************************
		 *            DYNAMIC		*
		 *******************************/

%!  cache_dynamic(:Head) is det.
%
%   Declare Head to be a dynamic predicate   that is stored in the cache
%   with a given mode. The mode arguments of   Head  are either +, or -.
%   For example:
%
%   ```
%   :- cache_dynamic
%       person(-),
%       age(+, -).
%   ```
%
%   The mode determines the variant under   which the facts are asserted
%   and should reflect the typical calling mode.  The goal may be called
%   using a more specific mode, e.g.,   person(bob), but the system will
%   retrieve the list of answers and perform a member/2 call to find the
%   answer.
%
%   This declaration makes the predicate  Head itself available, calling
%   cached/2 and allows calling the modification predicates:
%
%     - cache_assert/1
%     - cache_asserta/1
%     - cache_assertz/1
%     - cache_retract/1
%     - cache_retractall/1

cache_dynamic(Head) :-
    throw(error(context_error(nodirective, cache_dynamic(Head)), _)).

:- multifile
    dynamic_cached/6.

system:term_expansion((:- cache_dynamic(Heads)),
                      Clauses) :-
    phrase(cache_dynamic(Heads), Clauses).

cache_dynamic(Var) -->
    { var(var), !,
      instantiation_error(Var)
    }.
cache_dynamic((A,B)) -->
    cache_dynamic(A),
    cache_dynamic(B).
cache_dynamic(Head0) -->
    [ prolog_signature:hook_predicate_hash(M:General, Hash),
      cache_rocks:dynamic_cached(Head, M, In, Variant, Signature, VarTerm),
      (M:General :- cache_rocks:dynamic_fact(M:General, Hash))
    ],
    { prolog_load_context(module, M0),
      strip_module(M0:Head0, M, Head),
      functor(Head, Name, Arity),
      functor(General, Name, Arity),
      variant_sha1(M:Head, Hash),
      variant_map(Head, Hash, In, Variant, Signature, VarTerm)
    }.

variant_map(Head, Hash, In, Variant, Signature, VarTerm) :-
    Head =.. [Name|Modes],
    maplist(mode_map, Modes, InArgs, VariantArgs),
    var_args(Modes, InArgs, VarArgs),
    In =.. [Name|InArgs],
    Variant =.. [Name|VariantArgs],
    Signature =.. [Hash|VariantArgs],
    VarTerm =.. [v|VarArgs].

mode_map(+, In, In) :- !.
mode_map(-, _, _) :- !.
mode_map(A, _, _) :-
    domain_error(mode, A).

var_args([], _, []).
var_args([+|T], [_|In], VarArgs) :-
    !,
    var_args(T, In, VarArgs).
var_args([-|T], [H|In], [H|VarArgs]) :-
    var_args(T, In, VarArgs).

%!  cache_assert(+Fact) is det.
%!  cache_asserta(+Fact) is det.
%!  cache_assertz(+Fact) is det.

cache_assert(Fact) :-
    cache_modify(Fact, assertz).
cache_asserta(Fact) :-
    cache_modify(Fact, asserta).
cache_assertz(Fact) :-
    cache_modify(Fact, assertz).

cache_modify(M:Fact, How) :-
    rocks(DB),
    functor(Fact, Name, Arity),
    functor(Mode, Name, Arity),
    (   dynamic_cached(Mode, M, Fact, Variant, Signature, VarTerm)
    ->  true
    ;   permission_error(assert, cache, M:Fact)
    ),
    functor(Signature, Hash, _),
    get_time(Now),
    Cache = cache(M:Variant, [VarTerm], complete, Now, Hash),
    Action =.. [How, Cache],
    update(Action, DB, Signature),
    (   rocks_variant_c(Signature, M, Variant)
    ->  true
    ;   register_variant(new, Signature, Cache)
    ).

%!  cache_retract(:Fact) is nondet.
%
%   True when Fact appeared in the database and could be retracted.

cache_retract(Fact) :-
    call(Fact),
    cache_modify(Fact, retract).

%!  cache_retractall(:Fact) is det.
%
%   Retract all terms unifying with Fact.

cache_retractall(M:Fact) :-
    rocks(DB),
    functor(Fact, Name, Arity),
    functor(Mode, Name, Arity),
    (   dynamic_cached(Mode, M, Fact, _Variant, Signature, _VarTerm)
    ->  true
    ;   permission_error(assert, cache, M:Fact)
    ),
    rocks_delete(DB, Signature).


is_cache_dynamic(M:Variant) :-
    functor(Variant, Name, Arity),
    functor(Mode, Name, Arity),
    dynamic_cached(Mode, M, _Fact, _Generic, _Signature, _VarTerm),
    !.

:- public
    dynamic_fact/2,
    merge_dynamic/5.

dynamic_fact(Goal, Hash) :-
    rocks(DB),
    cached(sha1, fail, DB, Goal, Hash).

%!  update(Action, DB, Signature) is det.
%
%   Update DB using action on Signature.

update(Action, DB, Signature) :-
    setting(merge, true), !,
    rocks_merge(DB, Signature, Action).
update(Action, DB, Signature) :-
    (   rocks_get(DB, Signature, Cache0)
    ->  merge_dynamic(full, _Key, Cache0, [Action], Cache)
    ;   merge_dynamic(full, _Key, [], [Action], Cache)
    ),
    rocks_put(DB, Signature, Cache).


merge_dynamic(Kind, _Key, Answers, Action, _Result) :-
    debug(cache(merge), '~p merge ~p with ~p', [Kind, Answers, Action]),
    fail.
merge_dynamic(full, Key, [], [First|Rest], Result) :-
    !,
    arg(1, First, Cache),
    merge_dynamic(full, Key, Cache, Rest, Result).
merge_dynamic(Kind, _Key, Cache0, Actions, Cache) :-
    cache_answers(Cache0, Answers0),
    merge_answers(Kind, Answers0, Actions, Answers),
    set_answers_of_cache(Answers, Cache0, Cache1),
    (   last(Actions, Last)
    ->  arg(1, Last, LastCache),
        cache_time(LastCache, Time),
        set_time_of_cache(Time, Cache1, Cache)
    ;   Cache = Cache1
    ).

merge_answers(partial, Actions1, Actions2, Actions) :-
    debug(cache(partial), 'Partial merge ~p with ~p', [Actions1, Actions2]),
    append(Actions1, Actions2, Actions).
merge_answers(full, Initial, Actions, Result) :-
    foldl(action, Actions, Initial, Result).

action(asserta(Cache), Answers, Result) :-
    cache_answers(Cache, New),
    append(New, Answers, Result).
action(assertz(Cache), Answers, Result) :-
    cache_answers(Cache, New),
    append(Answers, New, Result).
action(retract(Cache), Answers, Result) :-
    cache_answers(Cache, Delete),
    subtract(Answers, Delete, Result).


		 /*******************************
		 *        TRACK VARIANTS	*
		 *******************************/

%!  rocks_variant(?Goal, :Signature)
%
%   True when the variant Goal is represented by the rocks cache.

rocks_variant(M:Goal, Signature) :-
    rocks_variant_cache,
    !,
    rocks_variant_c(Signature, M, Goal).
rocks_variant(M:Goal, Signature) :-
    with_mutex(cache_rocks, build_variant_cache),
    rocks_variant_c(Signature, M, Goal).

%!  rocks_predicate(?Goal, :Signature)
%
%   True when some variant of Goal is represented by the rocks cache.

rocks_predicate(M:Goal) :-
    rocks_variant_cache,
    !,
    rocks_predicate_c(M, Goal).
rocks_predicate(M:Goal) :-
    with_mutex(cache_rocks, build_variant_cache),
    rocks_predicate_c(M, Goal).

build_variant_cache :-
    rocks_variant_cache,
    !.
build_variant_cache :-
    rocks(DB),
    assert(rocks_variant_cache_enabled),
    forall(rocks_enum(DB, Signature, cache(M:Goal, _,_,_,_)),
           assert_variant(Signature, M, Goal)),
    assert(rocks_variant_cache).

assert_variant(Signature, M, Goal) :-
    assertz(rocks_variant_c(Signature, M, Goal)),
    (   rocks_predicate_c(M, Goal)
    ->  true
    ;   functor(Goal, Name, Arity),
        functor(Gen, Name, Arity),
        assertz(rocks_predicate_c(M, Gen))
    ).

%!  register_variant(+Update, +Signature, +Cache) is det.

register_variant(new, Signature, Cache) :-
    rocks_variant_cache_enabled, !,
    cache_variant(Cache, M:Goal),
    assert_variant(Signature, M, Goal).
register_variant(_, _, _).

%!  unregister_variant(+Signature, :Variant) is det.

unregister_variant(Signature, M:Variant) :-
    retractall(rocks_variant_c(Signature, M, Variant)),
    functor(Variant, Name, Arity),
    functor(Gen, Name, Arity),
    (   rocks_variant_c(_, M, Gen)
    ->  true
    ;   retractall(rocks_predicate_c(M, Gen))
    ).


%!  this_cache_property(:Goal, ?Property) is nondet.
%!  cache_property(:Goal, ?Property) is nondet.
%
%   Query properties of the cache.  These   predicates  vary  on what is
%   queried:
%
%     - this_cache_property/2 queries properties for exactly the
%       given goal variant.
%     - cache_property/2 queries properties for all goals subsumed
%       by Goal.
%
%   Defined properties are:
%
%     - current(-Bool)
%     If `true`, the predicate nor it callees have been changed
%     - count(-Count)
%     Number of answers
%     - state(-State)
%     Completion state.  One of `complete`, `partial` or exception(Ex).
%     - time_cached(-Time)
%     Time stamp when the cache was created.
%     - hash(-Hash)
%     Deep hash of the predicate associated with the goal.

cache_property(M:Goal, Property) :-
    rocks(DB),
    (   callable(Goal)
    ->  functor(Goal, Name, Arity),
        functor(Variant, Name, Arity)
    ;   true
    ),
    rocks_variant(M:Variant, Signature),
    subsumes_term(Goal, Variant),
    Goal = Variant,
    rocks_get(DB, Signature, Cache),
    property(Property, M:Goal, Cache).

this_cache_property(Goal, Property) :-
    rocks(DB),
    goal_signature(Goal, Signature, _Vars),
    rocks_get(DB, Signature, Cache),
    property(Property, Goal, Cache).

property(current(Bool), Variant, cache(_, _, _, _, Hash)) :-
    (   deep_predicate_hash(Variant, Hash)
    ->  Bool = true
    ;   Bool = false
    ).
property(count(Count), _, cache(_, Answers, _, _, _)) :-
    length(Answers, Count).
property(state(State), _, cache(_, _, State, _, _)).
property(time_cached(Time), _, cache(_, _, _, Time, _)).
property(hash(Hash), _, cache(_, _, _, _, Hash)).

%!  forget(:Goal)
%
%   Forget all cached results that are  subsumed by Goal. Typically used
%   as forget(m:p(_,_)) to remove all  data   cached  for m:p/2. Notably
%   forget(_:_) will destroy the entire cache.
%
%   @bug Despite using a RocksDB batch write, deleting many variants can
%   be really slow.

forget(M:Goal) :-
    rocks(DB),
    (   callable(Goal)
    ->  functor(Goal, Name, Arity),
        functor(Variant, Name, Arity)
    ;   true
    ),
    findall(delete(Signature),
            ( rocks_variant(M:Variant, Signature),
              subsumes_term(M:Goal, M:Variant)
            ), Batch),
    (   debugging(cache(forget))
    ->  length(Batch, Len),
        debug(cache(forget), 'Deleting ~D records~n', [Len])
    ;   true
    ),
    rocks_batch(DB, Batch),
    (   rocks_variant(M:Variant, Signature),
        subsumes_term(M:Goal, M:Variant),
        unregister_variant(Signature, M:Variant),
        fail
    ;   true
    ).

%!  cache_statistics(?Key)
%
%   True when Key is a know statistics about the caching mechanism.

cache_statistics(Property) :-
    rocks(DB),
    rocks_property(DB, Property).

%!  cache_listing is det.
%!  cache_listing(+Options) is det.
%
%   List contents of the persistent cache.  Options:
%
%     - hash(long)
%       Show full SHA1 hash rather than short (7 char) hashes
%     - max_variants(N)
%       Max number of variants per predicate to show.  Default 10.
%     - time_format(Format)
%       format_time/3 spec for displaying the time.  Default `"%FT%T"`.
%
%   The listing shows for each variant:
%
%     1. The call variant
%     2. Time it was created
%     3. Hash under which the variant is registered
%     4. State, which is one of
%        - *C* complete
%        - *D* dynamic
%        - *P* partial
%        - *E* exception
%     5. Number of answers for this variant

cache_listing :-
    cache_listing([]).

cache_listing(Options) :-
    rocks_d(_,_),
    columns(Cols, Options),
    !,
    sum_list(Cols, CL),
    format('Predicate ~t Created~*| ~tHash State~*+ ~t Count~*+~n', Cols),
    format('~`=t~*|~n', [CL]),
    Predicate = _:_,
    forall(order_by([asc(Predicate)], rocks_predicate(Predicate)),
           list_predicate_cache(Predicate, Cols, Options)).
cache_listing(_) :-
    format('No persistent answer cache.  Use cache_open/1 to create one.~n').

columns([55,47,7], Options) :-
    option(hash(long), Options),
    !.
columns([55,14,7], _).

list_predicate_cache(Predicate, Cols, Options) :-
    Predicate = M:Head,
    functor(Head, Name, Arity),
    findall(Predicate-Signature, rocks_variant(Predicate, Signature), Pairs),
    keysort(Pairs, Sorted),
    report(M:Name/Arity, Sorted, Cols, Options).

report(M:Name/Arity, Variants, Cols, Options) :-
    length(Variants, VCount),
    option(max_variants(Max), Options, 10),
    format('~w:~w/~d (~D variants)~n', [M, Name, Arity, VCount]),
    forall(limit(Max, member(Variant-Signature, Variants)),
           report_variant(Variant, Signature, Cols, Options)),
    Skipped is VCount - Max,
    (   Skipped > 0
    ->  format('  (skipped ~D variants)~n', [Skipped])
    ;   true
    ).

report_variant(Variant, Signature, [C1,C2,C3], Options) :-
    C23 is C2 + C3,
    rocks(DB),
    option(time_format(TF), Options, "%FT%T"),
    rocks_get(DB, Signature, Cache),
    Cache = cache(Variant, Answers, State, Time, Hash),
    state_flags(State, Variant, Comp),
    current(Hash, Variant, Current),
    short_hash(Hash, ShortHash, Options),
    format_time(string(Date), TF, Time),
    numbervars(Variant, 0, _, [singletons(true)]),
    length(Answers, Count),
    format('  ~p ~`.t ~s~*| ~w ~w~w ~`.t ~D~*+~n',
           [ Variant, Date, C1,
             ShortHash, Comp,Current,
             Count, C23
           ]).

state_flags(complete,     Variant, F) :-
    (   is_cache_dynamic(Variant)
    ->  F = 'D'
    ;   F = 'C'
    ).
state_flags(partial,      _, 'P').
state_flags(exception(_), _, 'E').

current(Hash, Variant, C) :-
    (   deep_predicate_hash(Variant, Hash)
    ->  C = '*'
    ;   C = ' '
    ).

short_hash(Hash, Hash, Options) :-
    option(hash(long), Options),
    !.
short_hash(Hash, Short, _) :-
    sub_string(Hash, 0, 7, _, Short).

:- multifile prolog:error_message//1.

prolog:error_message(consistency_error(Goal, Template, First)) -->
    [ '~p yielded inconsistent results (~p \\=@= ~p)'-[Goal, Template, First] ].
prolog:error_message(specific_expected(Goal, Expected, _Hash)) -->
    [ '~p is not a specialization of ~p'-[Goal, Expected] ].
prolog:error_message(pcache(no_db)) -->
    [ 'pcache: No persistent answer cache.  Use cache_open/1 to create one'-[] ].

:- multifile sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(cache_rocks:cached/1).
sandbox:safe_meta_predicate(cache_rocks:forget/1).
