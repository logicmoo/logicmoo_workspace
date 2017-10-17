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
                      instance/1, (declaration)/1, (declaration)/2, check/1,
                      trust/1, true/1, false/1]).

:- use_module(library(assertions)).

:- true prop (type)/1 + (declaration(check), global(prop)) # "Defines a type.".

type(Goal) :- call(Goal).

:- true prop (global)/1 + (global(prop), declaration)
# "A property that is global, i.e., can appear after the + in the assertion.
and as meta predicates, meta_predicate F(0) (assrt_lib.pl)".

global(Goal) :- call(Goal).

:- global global(Prop, Goal) : (assrt_type(Prop), callable(Goal))
# "Like global/1, but allows to specify the default assertion type".

global(_, Goal) :- call(Goal).

:- true prop (declaration)/1 + (global(prop), declaration)
# "A property that is a declaration, i.e., an operator is added as op(1125, fx, F). Implies global/1".

declaration(Goal) :- call(Goal).

:- true prop declaration(Goal, Status) : (callable(Goal), assrt_status(Status)) + global(prop)
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

:- meta_predicate compat(0).

compat(_:H) :-
    % This first clause allows usage of atom/atomic and other test predicates as
    % compatibility check
    compound(H),
    compatc(H), !.
compat(Goal) :-
    copy_term_nat(Goal, Term),
    \+ \+ do_compat(Term).

do_compat(Goal) :-
    term_variables(Goal, VS),
    prolog_current_choice(CP),
    maplist(freeze_cut(CP), VS),
    Goal.

freeze_cut(CP, V) :-
    freeze(V, catch(prolog_cut_to(CP), _, true)).

compatc(H) :-
    functor(H, _, N),
    arg(N, H, A),
    var(A), !.
compatc(var(_)).
compatc(nonvar(_)).
compatc(term(_)).
compatc(gnd(_)).
compatc(ground(_)).

freeze_fail(CP, V) :-
    freeze(V, ( prolog_cut_to(CP),
                fail
              )).

:- global instance(Prop)
# "Uses Prop as an instantiation property. Verifies that execution of
   ~w does not produce bindings for the argument variables."-[Prop].

:- meta_predicate instance(0).

instance(Goal) :-
    term_variables(Goal, VS),
    prolog_current_choice(CP),
    \+ \+ ( maplist(freeze_fail(CP), VS),
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
