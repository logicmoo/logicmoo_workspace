:- module(p2, []).

:- use_module(library(assertions)).
:- use_module(library(plprops)).
:- use_module(library(rtchecks)).
:- use_module(p3).

:- rtcheck p/1.

:- pred p(int).

p(1).
p(2).
p(3).

q :-
    p3:p(a).

