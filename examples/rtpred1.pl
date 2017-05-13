:- module(rtpred1, [p1/1]).

:- use_module(library(rtchecks)).
:- use_module(library(assertions)).
:- use_module(library(plprops)).

:- rtchecked p1/1.

:- pred p1(atm).
:- pred p1/1 is det.

p1(a).
p1(b).
p1(c).
p1(1).
