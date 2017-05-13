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
                      trust/1, true/1, false/1, add_1st_arg/3]).

:- use_module(library(assertions)).

:- true prop (type)/1 + (declaration(check), global(prop)) # "Defines a type.".

type(Goal) :- call(Goal).

:- true prop (global)/1 + (global(prop), declaration)
# "A property that is global, i.e., can appear after the + in the assertion.
and as meta predicates, meta_predicate F(0) (assrt_lib.pl)".

global(Goal) :- call(Goal).

:- global global(Goal, Prop) : (callable(Goal), assrt_type(Prop))
# "Like global/1, but allows to specify the default assertion type".

global(Goal, _) :- call(Goal).

:- true prop (declaration)/1 + (global(prop), declaration)
# "A property that is a declaration, i.e., an operator is added as op(1125, fx, F). Implies global/1".

declaration(Goal) :- call(Goal).

:- true prop declaration(Goal, Status) : (callable(Goal), assrt_status(Status)) + global(prop)
# "Like declaration/1, but allows to specify the default assertion status".

declaration(Goal, _) :- call(Goal).

:- true prop type(X, Y)
# "~w is internally of type ~w (@tt{var}, @tt{attv}, @tt{float},
      @tt{integer}, @tt{structure}, @tt{atom} or @tt{list})."-[X, Y].
:- true comp type(X, _) : nonvar(X) + eval.
:- meta_predicate type(?, :).

type(A, T) :-
    add_1st_arg(T, A, P),
    call(P).

add_1st_arg(M:T, A, M:P) :- !,
    add_1st_arg(T, A, P).
add_1st_arg(T, A, P) :-
    T =.. [F|Args],
    P =.. [F, A|Args].

% Help analyzers to identify this call:
prolog:called_by(type(A, MT), metaprops, CM, [M:P]) :-
    nonvar(A),
    nonvar(MT),
    strip_module(CM:MT, M, T),
    nonvar(T),
    add_1st_arg(T, A, P).

:- multifile
    unfold_calls:unfold_call_hook/4.

unfold_calls:unfold_call_hook(type(A, MT), metaprops, CM, M:P) :-
    strip_module(CM:MT, M, T),
    nonvar(T),
    add_1st_arg(T, A, P).

:- true prop compat(Prop)
# "Uses ~w as a compatibility property."-[Prop].

:- meta_predicate compat(0).

compat(_:H) :-
    % This first clause allows usage of atom/atomic and other test predicates as
    % compatibility check
    compound(H),
    compatc(H), !.
compat(Goal) :- \+ \+ Goal.

compatc(H) :-
    arg(1, H, A),
    var(A), !.
compatc(var(_)).
compatc(nonvar(_)).
compatc(term(_)).
compatc(gnd(_)).
compatc(ground(_)).

:- global instance(Prop)
# "Uses Prop as an instantiation property. Verifies that execution of
   ~w does not produce bindings for the argument variables."-[Prop].

instance(Goal) :-
    term_variables(Goal, VS),
    Goal,
    term_variables(Goal, VO),
    ( VS == VO
    ->true
    ; !,
      fail
    ).

:- meta_predicate check(0).
check(_).

:- meta_predicate trust(0).
trust(_).

:- meta_predicate true(0).
true(_).

:- meta_predicate false(0).
false(_).
