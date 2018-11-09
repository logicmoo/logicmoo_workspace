/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(metaprops, [(type)/1, (type)/2, (global)/1, (global)/2, compat/1,
                      compat/2, instan/1, instan/2, (declaration)/1,
                      (declaration)/2, check/1, trust/1, true/1, false/1,
                      last_prop_failure/1]).

:- use_module(library(assertions)).
:- use_module(library(resolve_calln)).
:- use_module(library(qualify_meta_goal)).

:- true prop (type)/1 + (declaration(check), global(prop)) # "Defines a type.".

type(Goal) :- call(Goal).

:- true prop (global)/1 + (global(prop), declaration)
# "A property that is global, i.e., can appear after the + in the assertion.
and as meta predicates, meta_predicate F(0) (assrt_lib.pl)".

global(Goal) :- call(Goal).

:- type assrt_type/1, assrt_status/1.

:- global global(Prop, Goal) : (assrt_type(Prop), callable(Goal))
# "Like global/1, but allows to specify the default assertion type".

global(_, Goal) :- call(Goal).

:- true prop (declaration)/1 + (global(prop), declaration)
# "A property that is a declaration, i.e., an operator is added as op(1125, fx, F). Implies global/1".

declaration(Goal) :- call(Goal).

:- true prop declaration(Status, Goal) : (assrt_status(Status), callable(Goal)) + global(prop)
# "Like declaration/1, but allows to specify the default assertion status".

declaration(_, Goal) :- call(Goal).

:- true prop type(T, A)
# "~w is internally of type ~w, a predicate of arity 1 defined as a type/1."-[A, T].
:- meta_predicate type(1, ?).

type(T, A) :- call(T, A).


:- multifile
    unfold_calls:unfold_call_hook/4.

unfold_calls:unfold_call_hook(type(T, A), metaprops, M, M:call(T, A)).

:- true prop compat(Prop)
# "Uses ~w as a compatibility property."-[Prop].

:- meta_predicate compat(0 ).

compat(M:Goal) :-
    term_variables(Goal, VS),
    compat(M:Goal, VS).

:- use_module(library(gcb)).
:- use_module(library(list_sequence)).
:- use_module(library(substitute)).
:- use_module(library(clambda)).
:- use_module(library(terms_share)).

:- dynamic
        '$last_prop_failure'/2.

generalize_term(STerm, Term, _) :-
    \+ terms_share(STerm, Term).

current_prop_failure((SG :- Body)) :-
    '$last_prop_failure'(Term, SubU),
    sort(SubU, Sub),
    greatest_common_binding(Term, Sub, ST, SSub, [[]], Unifier, []),
    substitute(generalize_term(SSub), ST, SG),
    maplist(\ A^(\+A)^true, SSub, NSub),
    foldl(simplify_unifier(SG-SSub), Unifier, LitL, NSub),
    LitL \= [],
    list_sequence(LitL, Body).

simplify_unifier(Term, A=B) -->
    ( {occurrences_of_var(A, Term, 0 )}
    ->{A=B}
    ; [A=B]
    ).

last_prop_failure(L) :-
    findall(E, once(current_prop_failure(E)), L),
    retractall('$last_prop_failure'(_, _)).

asserta_prop_failure(T, S) :-
    once(retract('$last_prop_failure'(T, L))),
    asserta('$last_prop_failure'(T, [S|L])).

cleanup_prop_failure(T, S) :-
    retractall('$last_prop_failure'(_, _)),
    asserta('$last_prop_failure'(T, S)).


compat(M:Goal, VarL) :-
    copy_term_nat(Goal-VarL, Term-VarTL), % get rid of corroutining while checking compatibility
    sort(VarTL, VS),
    cleanup_prop_failure(Term, []),
    prolog_current_choice(CP),
    compat(Term, data(VS, Term, CP), M).

% this small interpreter will reduce the possibility of loops if the goal being
% checked is not linear, i.e., if it contains linked variables:
compat(Var, _, _) :- var(Var), !.
compat(M:Goal, D, _) :-
    !,
    compat(Goal, D, M).
compat(A, D, M) :-
    do_resolve_calln(A, B),
    !,
    compat(B, D, M).
compat((A, B), D, M) :-
    !,
    compat(A, D, M),
    compat(B, D, M).
compat(compat(A), D, M) :-
    !,
    compat(A, D, M).
compat((A->B; C), D, M) :-
    !,
    ( call(M:A)
    ->compat(B, D, M)
    ; compat(C, D, M)
    ),
    !.
compat((A->B), D, M) :-
    !,
    ( call(M:A)
    ->compat(B, D, M)
    ).
compat(!, data(_, _, CP), _) :-
    !,
    cut_from(CP).
compat(A, data(VarL, _, _), M) :-
    % This clause allows usage of simple test predicates as compatibility check
    compound(A),
    A \= (_;_),
    compatc(A, VarL, M),
    !.
compat(Term, D, M) :-
    D = data(_, T, _),
    asserta_prop_failure(T, Term),
    compat_1(Term, D, M),
    cleanup_prop_failure(T, []).

% NOTE: The cut in compat_1 assume that is safe to do it.  That happens when the
% arguments of the Goal do not share with other parts of the check that could
% eventually lead the execution to a failure and backtrack.

compat_1((A; B), D, M) :-
    ( compat(A, D, M)
    ; compat(B, D, M)
    ),
    !.
compat_1(A, D, M) :-
    ( is_prop(A, M)
    ->catch(compat_body(M:A, D),
            _,
            do_compat(M:A, D))
    ; do_compat(M:A, D)
    ),
    !.

do_compat(Goal, data(VarL, _, _)) :-
    term_variables(VarL, VS),
    prolog_current_choice(CP),
    maplist(freeze_cut(CP), VS),
    Goal,
    maplist(del_freeze, VS).

del_freeze(Var) :-
    ( attvar(Var)
    ->del_attr(Var, freeze)
    ; true
    ).

is_prop(Head, M) :-
    prop_asr(Head, M, Stat, prop, _, _, _),
    memberchk(Stat, [check, true]).

:- meta_predicate compat_body(0, +).

compat_body(MG1, data(V, T, _)) :-
    qualify_meta_goal(MG1, MG),
    prolog_current_choice(CP),
    clause(MG, Body, Ref),
    clause_property(Ref, module(CM)),
    compat(Body, data(V, T, CP), CM).

:- use_module(library(safe_prolog_cut_to)).

cut_from(CP) :- catch(safe_prolog_cut_to(CP), _, true).

freeze_cut(CP, V) :-
    freeze(V, catch(prolog_cut_to(CP), _, true)).

compatc(H, VarL, M) :-
    functor(H, _, N),
    arg(N, H, A),
    ( var(A),
      ord_intersect(VarL, [A], [A])
    ; predicate_property(M:H, meta_predicate(Spec)),
      arg(N, Spec, Meta),
      integer(Meta),
      Meta>=0,
      A = X:Y,
      ( ( var(X)
        ; current_module(X)
        )
      ->var(Y),
        ord_intersect(VarL, [Y], [Y])
      )
    ),
    !.
compatc(H, VarL, _) :-
    compatc_arg(H, A),
    (var(A)->ord_intersect(VarL, [A], [A]) ; true).

compatc_arg(var(      A), A).
compatc_arg(nonvar(   A), A).
compatc_arg(term(     A), A).
compatc_arg(gnd(      A), A).
compatc_arg(ground(   A), A).
compatc_arg(nonground(A), A).

freeze_fail(CP, Term, V) :-
    freeze(V, ( prolog_cut_to(CP),
                cleanup_prop_failure(Term, [V]),
                fail
              )).

:- global instan(Prop)
# "Uses Prop as an instantiation property. Verifies that execution of
   ~w does not produce bindings for the argument variables."-[Prop].

:- meta_predicate instan(0).

instan(Goal) :-
    term_variables(Goal, VS),
    instan(Goal, VS).

:- meta_predicate instan(0, +).

instan(Goal, VS) :-
    prolog_current_choice(CP),
    \+ \+ ( maplist(freeze_fail(CP, Goal), VS),
            Goal
          ).

:- meta_predicate check(0).
check(_).

:- meta_predicate trust(0).
trust(_).

:- meta_predicate true(0).
true(_).

:- meta_predicate false(0).
false(_).
