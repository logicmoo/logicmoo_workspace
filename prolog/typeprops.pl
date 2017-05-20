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

:- module(typeprops, [list/1, list/2, tlist/2, nlist/2, int/1, nnegint/1,
                      posint/1, flt/1, num/1, atm/1, gnd/1, str/1, gndstr/1,
                      constant/1, term/1, char/1, pair/1, struct/1, sequence/2,
                      character_code/1, linear/1, sequence_or_list/2, keypair/1,
                      is_pred/2, any/1, mod_qual/1, mod_qual/2, keylist/1,
                      arithexpression/1, predname/1, operator_specifier/1]).

:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(apply)).
:- use_module(library(static_strip_module)).

%!  int(Int)
%
%   The type of integers

:- type int/1.

int(X) :-
    nonvar(X), !,
    integer(X).
int(0).
int(X) :-
    posint(N),
    give_sign(N, X).

:- type posint/1.

posint(1).
posint(N) :-
    posint(N1),
    succ(N1, N).

give_sign(P, P).
give_sign(P, N) :- N is -P.

:- type nnegint/1.

%!  nnegint(X)
%
%   The type of non negative integers, i.e., natural numbers (including zero)

nnegint(X) :-
    nonvar(X), !,
    integer(X),
    X >= 0.
nnegint(0).
nnegint(N) :- posint(N).

:- type flt/1.

%!  flt(X)
%
%   Floating point numbers

flt(X) :-
    nonvar(X), !,
    float(X).
flt(F) :-
    nnegint(X),
    ( Q is 1.0*X
    ; frac(X, Q)
    ),
    give_sign(Q, F).

:- type num/1.

%!  num(X)
%
%   Numbers

num(X) :-
    nonvar(X), !,
    number(X).
num(F) :-
    nnegint(X),
    ( Q is X
    ; Q is 1.0*X
    ; frac(X, Q)
    ),
    give_sign(Q, F).

frac(X, Q) :-
    between(2, X, Y),
    1 =:= gcd(X, Y),
    ( Q is X/Y
    ; Q is Y/X
    ).

:- type atm/1.

%!  atm(A)
%
%   An atom

atm(T) :- nonvar(T), !, atom(T).
atm(A) :-
    list(character_code, L),
    atom_codes(A, L).

:- type str/1.

%!  str(S)
%
%   A string

str(T) :- nonvar(T), !, string(T).
str(S) :-
    list(character_code, L),
    string_codes(S, L).

:- type character_code/1 # "an integer which is a character code.".

character_code(I) :- between(0, 255, I).

:- type constant/1 # "An atomic term (an atom, string or a number).".

constant([]).
constant(T) :- atm(T).
constant(T) :- num(T).
constant(T) :- str(T).

:- type predname/1.

%!  predname(PI)
%
%   A predicate indicator

predname(P/A) :-
    atm(P),
    nnegint(A).

:- type term/1 # "Any term".

term(_).

:- type list(L) # "~w is a list."-[L].

list([]).
list([_|L]) :- list(L).

:- type list(T, L) # "~w is a list of ~ws."-[L, T].
:- meta_predicate list(1, ?).

list(Type, List) :- maplist(Type, List).

:- type pair/1.
pair(_-_).

:- type keypair/1.
keypair(_-_).

:- type keylist/1.
keylist(KL) :- list(keypair, KL).

:- type tlist/2 # "@var{L} is a list or a value of @var{T}s".
:- meta_predicate tlist(?, 1).
tlist(L, T) :- list(T, L).
tlist(E, T) :- type(T, E).

:- type nlist/2 # "A nested list".
:- meta_predicate nlist(1, ?).

nlist(Type, NList) :- nlist_(NList, Type).

nlist_([], _).
nlist_([X|Xs], T) :-
        nlist_(X, T),
        nlist_(Xs, T).
nlist_(X, T) :-
        type(T, X).

:- type char/1.
char(A) :- atm(A). % size(A)=1

:- type any/1.
any(_).

:- type linear/1
# "A linear term, i.e. all variables occurrs only once.".

linear(T) :-
    term_variables(T, Vars),
    maplist(occurrs_one(T), Vars).

occurrs_one(T, Var) :- occurrences_of_var(Var, T, 1).

:- type sequence(T, S) # "~w is a sequence of ~ws."-[S, T].

:- meta_predicate sequence(1, ?).

sequence(T, S) :- sequence_(T, S).

sequence_(E, T) :- type(E, T).
sequence_((E, S), T) :-
        type(E, T),
        sequence_(S, T).

:- type sequence_or_list/2.
:- meta_predicate sequence_or_list(1, ?).

sequence_or_list(T, E) :- list(T, E).
sequence_or_list(T, E) :- sequence(T, E).

:- type struct/1 # "A compound term".

% TBD: Proper generator
struct([_|_]):- !.
struct(T) :- functor(T, _, A), A>0. % compound(T).

:- type gnd/1 # "A ground term".

% TBD: Proper generator
gnd([]) :- !.
gnd(T) :-
    term_variables(T, Vars),
    maplist(gnd, Vars).

:- type arithexpression/1
    # "Represents an arithmetic expression, i.e., a term that could be
    an argument for an arithmetic predicate.".

:- type gndstr/1.

gndstr(A) :- gnd(A), struct(A).

arithexpression(X) :- number(X), !. % Optimization
arithexpression(X) :- num(X).
arithexpression(X) :-
    callable(X),
    current_arithmetic_function(X),
    X =.. [_|Args],
    maplist(arithexpression, Args).

% BUG: if the trace have all the ports active, we can not use ';'/2 in is_pred/2
% and some variables becomes uninstantiated. That is an SWI-Prolog bug but I
% don't have time to isolate it --EMM

:- true prop is_pred(N, P)
    # "check that ~w is a defined predicate with ~w extra arguments."-[P, N].
:- meta_predicate is_pred(?, :).
is_pred(N, Pred) :-
    nnegint(N),
    is_pred_2(Pred, N).

is_pred_2(M:Pred, N) :-
    var(Pred), !,
    current_predicate(M:F/A),
    A >= N,
    A1 is A - N,
    functor(Pred, F, A1).
is_pred_2(M:Pred, N) :-
    functor(Pred, F, A1),
    A is A1 + N,
    current_predicate(M:F/A).

:- true prop mod_qual/1.
mod_qual(M:V) :-
    static_strip_module(V, M, _, CM),
    current_module(CM).

:- true prop mod_qual/2.
:- meta_predicate mod_qual(:, ?).
mod_qual(T, M:V) :-
    static_strip_module(V, M, C, CM),
    current_module(CM),
    type(T, C).

:- type operator_specifier/1.

operator_specifier(fy).
operator_specifier(fx).
operator_specifier(yfx).
operator_specifier(xfy).
operator_specifier(xfx).
operator_specifier(yf).
operator_specifier(xf).

:- use_module(library(implementation_module)).
:- use_module(library(unfold_calls)).

unfoldable(list(_, _),     typeprops).
unfoldable(nlist(_, _),    typeprops).
unfoldable(sequence(_, _), typeprops).

prolog:called_by(Goal, typeprops, CM, CL) :-
    nonvar(Goal),
    implementation_module(CM:Goal, M),
    unfoldable(Goal, M),
    unfold_calls(Goal, CM, unfoldable, CL).
