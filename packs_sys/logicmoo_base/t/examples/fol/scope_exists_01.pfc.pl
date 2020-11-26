#!/usr/bin/env swipl

:- module(t123,[]).

:- include(test_header).
:- module(t123).

:- dynamic(t123:ttExpressionType/1).

% :- process_this_script.

:- statistics.

:- test_boxlog(all(R,exists(D,implies(room(R), and(door(D), has(R, D)))))).


:- test_boxlog(all(R,implies(room(R),exists(D,and(door(D), has(R, D)))))).


:- test_boxlog(exists(D, all(R, implies(room(R), and(door(D), hasShared(R, D)))))).


