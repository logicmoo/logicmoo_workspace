#!/usr/bin/env swipl

:- module(sm1,[]).

:- meta_predicate(like_bag(^,-)).

:- ensure_loaded(library(pfc)).

like_bag(I,O):-copy_term(I,O).

'==>'(a,b).

pfcControlled(c).

c :- a.
